package cn.blmdz.home.session.redis;

import cn.blmdz.home.session.redis.JedisCallback;
import cn.blmdz.home.session.util.Configuration;

import com.google.common.base.Splitter;
import com.google.common.collect.Sets;

import java.util.Set;

import redis.clients.jedis.Jedis;
import redis.clients.jedis.JedisPool;
import redis.clients.jedis.JedisPoolConfig;
import redis.clients.jedis.JedisSentinelPool;
import redis.clients.util.Pool;

public class JedisPoolExecutor {
   private volatile Pool jedisPool;

   public JedisPoolExecutor(JedisPoolConfig config, boolean sentinel, Configuration configuration) {
      if(sentinel) {
         String sentinelProps = configuration.getSessionRedisSentinelHosts();
         Iterable<String> parts = Splitter.on(',').trimResults().omitEmptyStrings().split(sentinelProps);
         Set<String> sentinelHosts = Sets.newHashSet(parts);
         String masterName = configuration.getSessionRedisSentinelMasterName();
         this.jedisPool = new JedisSentinelPool(masterName, sentinelHosts, config);
      } else {
         String redisHost = configuration.getSessionRedisHost();
         int redisPort = configuration.getSessionRedisPort().intValue();
         this.jedisPool = new JedisPool(config, redisHost, redisPort);
      }

   }

   public Object execute(JedisCallback cb) {
      return this.execute(cb, 0);
   }

   public Object execute(JedisCallback cb, int index) {
      Jedis jedis = null;

      Object var4;
      try {
         jedis = (Jedis)this.jedisPool.getResource();
         jedis.select(index);
         var4 = cb.execute(jedis);
      } finally {
         if(jedis != null) {
            jedis.close();
         }

      }

      return var4;
   }

   public Pool getJedisPool() {
      return this.jedisPool;
   }
}
