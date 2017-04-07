package cn.blmdz.rabbit.settlement.impl.dao.redis;

import java.util.List;
import java.util.Set;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import cn.blmdz.home.common.redis.utils.JedisTemplate;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import redis.clients.jedis.Jedis;
import redis.clients.jedis.Transaction;

/**
 * Created with IntelliJ IDEA
 * Author: songrenfei
 * Date: 3/28/16
 * Time: 2:19 PM
 */
@Repository
@Slf4j
public class SettlementRedisDao {

    @Autowired
    @Setter
    private JedisTemplate jedisTemplate;

    /**
     * 将对于业务类型的记录id用逗号分隔保存到redis中
     * @param businessType 业务类型 {@link cn.blmdz.rabbit.settlement.enums.SettlementRedisBusinessType}
     * @param ids 逗号分隔
     */
    public void generateRedisIds(final Integer businessType, final List<Long> ids){
        jedisTemplate.execute(new JedisTemplate.JedisActionNoResult() {
            @Override
            public void action(Jedis jedis) {
                Transaction t = jedis.multi();

                for (Long id : ids){
                    t.sadd(keyOfIds(businessType), id.toString());
                }
                t.exec();
            }

        });

    }

    public Set<String> getRedisIds(final Integer businessType){
       return jedisTemplate.execute(new JedisTemplate.JedisAction<Set<String>>() {
            @Override
            public Set<String> action(Jedis jedis) {
                return jedis.smembers(keyOfIds(businessType));
            }
        });
    }


    public void delRedisIds(final Integer businessType){
         jedisTemplate.execute(new JedisTemplate.JedisActionNoResult() {
            @Override
            public void action(Jedis jedis) {
                 jedis.del(keyOfIds(businessType));
            }
        });
    }


    private String keyOfIds(Integer type) {
        return "settlement:ids:type:" + type;
    }

}
