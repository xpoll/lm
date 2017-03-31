package cn.blmdz.home.session.redis;

import redis.clients.jedis.Jedis;

public interface JedisCallback {
   Object execute(Jedis var1);
}
