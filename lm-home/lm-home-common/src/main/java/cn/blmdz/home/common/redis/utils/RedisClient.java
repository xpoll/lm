package cn.blmdz.home.common.redis.utils;

import com.google.common.base.Function;
import com.google.common.collect.Lists;

import cn.blmdz.home.common.redis.utils.JedisTemplate;
import cn.blmdz.home.common.redis.utils.JedisTemplate.JedisAction;
import redis.clients.jedis.Jedis;
import redis.clients.jedis.Pipeline;

import java.util.Iterator;
import java.util.List;

/**
 * email:dong_peiji@huateng.com
 * Created by 董培基 on 2016/3/3.
 */
public abstract class RedisClient {

    /**
     * 通过 key 获取value的长度
     *
     * @param jedisTemplate 注入的jedis模板
     * @param key           key键
     * @return 长度
     */
    public static Long listLen(JedisTemplate jedisTemplate, final String key) {
        return jedisTemplate.execute(new JedisAction<Long>() {
            @Override
            public Long action(Jedis jedis) {
                //返回list map set的长度
                return jedis.llen(key);
            }
        });
    }

    public static List<Long> listAll2Long(JedisTemplate jedisTemplate, final String key) {
        return (List<Long>) jedisTemplate.execute(new JedisAction() {
            public List<Long> action(Jedis jedis) {
                List<String> strVals = jedis.lrange(key, 0L, -1L);
                List<Long> longVals = Lists.transform(strVals, new Function<String, Long>() {
                    @Override
                    public Long apply(String s) {
                        return Long.parseLong(s);
                    }
                });
                return longVals;
            }
        });
    }

    public static Long listRemOne(JedisTemplate jedisTemplate, String key, Object val) {
        return listRem(jedisTemplate, key, val, 1L);
    }

    public static Long listRemAll(JedisTemplate jedisTemplate, String key, Object val) {
        return listRem(jedisTemplate, key, val, 0L);
    }


    private static Long listRem(JedisTemplate jedisTemplate, final String key, final Object val, final Long count) {
        return (Long) jedisTemplate.execute(new JedisAction() {
            public Long action(Jedis jedis) {
                return jedis.lrem(key, count, String.valueOf(val));
            }
        });
    }

    public static Long listRemOne(JedisTemplate jedisTemplate, List<String> keys, Object val) {
        return listRem(jedisTemplate, keys, val, 1L);
    }

    public static Long listRemAll(JedisTemplate jedisTemplate, List<String> keys, Object val) {
        return listRem(jedisTemplate, keys, val, 0L);
    }

    private static Long listRem(JedisTemplate jedisTemplate, final List<String> keys, final Object val, final Long count) {
        return jedisTemplate.execute(new JedisAction<Long>() {
            @Override
            public Long action(Jedis jedis) {
                Pipeline p = jedis.pipelined();
                Long deleted = 0L;

                String key;
                for(Iterator<String> var4 = keys.iterator(); var4.hasNext(); deleted = deleted + p.lrem(key, count, String.valueOf(val)).get()) {
                    key = var4.next();
                }

                p.sync();
                return deleted;
            }
        });
    }


}
