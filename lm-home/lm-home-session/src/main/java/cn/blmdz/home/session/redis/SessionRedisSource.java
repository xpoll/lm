package cn.blmdz.home.session.redis;

import cn.blmdz.home.session.AFSession;
import cn.blmdz.home.session.SessionDataSource;
import cn.blmdz.home.session.exception.SessionException;
import cn.blmdz.home.session.redis.JedisCallback;
import cn.blmdz.home.session.redis.JedisPoolExecutor;
import cn.blmdz.home.session.serialize.JsonSerializer;
import cn.blmdz.home.session.serialize.KryoSerializer;
import cn.blmdz.home.session.serialize.Serializer;
import cn.blmdz.home.session.util.Configuration;

import com.google.common.base.Strings;
import com.google.common.base.Throwables;

import java.util.Collections;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import redis.clients.jedis.Jedis;
import redis.clients.jedis.JedisPoolConfig;

public class SessionRedisSource implements SessionDataSource {
   private static final Logger log = LoggerFactory.getLogger(SessionRedisSource.class);
   private volatile JedisPoolExecutor executor;
   private Serializer serializer;
   private int dbIndex;

   public SessionRedisSource(Configuration configuration) {
      this.dbIndex = configuration.getSessionRedisDbIndex().intValue();
      if("binary".equals(configuration.getSerializeType())) {
         this.serializer = new KryoSerializer();
      } else {
         this.serializer = new JsonSerializer();
      }

      JedisPoolConfig config = new JedisPoolConfig();
      config.setTestOnBorrow(true);
      config.setMaxIdle(configuration.getSessionRedisMaxIdle().intValue());
      config.setMaxTotal(configuration.getSessionRedisMaxTotal().intValue());
      if(configuration.isCluster().booleanValue()) {
         this.executor = new JedisPoolExecutor(config, true, configuration);
      } else {
         this.executor = new JedisPoolExecutor(config, false, configuration);
      }

   }

   public Map findSessionById(String prefix, String id) {
      final String sessionId = prefix + ":" + id;

      try {
         return (Map)this.executor.execute(new JedisCallback() {
            public Map execute(Jedis jedis) {
               String session = jedis.get(sessionId);
               return !Strings.isNullOrEmpty(session)?SessionRedisSource.this.serializer.deserialize(session):Collections.emptyMap();
            }
         }, this.dbIndex);
      } catch (Exception var5) {
         log.error("failed to find session(key={}) in redis,cause:{}", sessionId, Throwables.getStackTraceAsString(var5));
         throw new SessionException("get session failed", var5);
      }
   }

   public void refreshExpireTime(AFSession afSession, int maxInactiveInterval) {
      this.doRefreshExpireTime(afSession.getPrefix() + ":" + afSession.getId(), maxInactiveInterval);
   }

   public void refreshExpireTime(String prefix, String id, int maxInactiveInterval) {
      this.doRefreshExpireTime(prefix + id, maxInactiveInterval);
   }

   public void doRefreshExpireTime(final String sessionId, final int maxInactiveInterval) {
      try {
         this.executor.execute(new JedisCallback() {
            public Void execute(Jedis jedis) {
               jedis.expire(sessionId, maxInactiveInterval);
               return null;
            }
         }, this.dbIndex);
      } catch (Exception var4) {
         log.error("failed to refresh expire time session(key={}) in redis,cause:{}", sessionId, Throwables.getStackTraceAsString(var4));
      }

   }

   public void deletePhysically(String prefix, String id) {
      final String sessionId = prefix + ":" + id;

      try {
         this.executor.execute(new JedisCallback() {
            public Void execute(Jedis jedis) {
               jedis.del(sessionId);
               return null;
            }
         }, this.dbIndex);
      } catch (Exception var5) {
         log.error("failed to delete session(key={}) in redis,cause:{}", sessionId, Throwables.getStackTraceAsString(var5));
      }

   }

   public boolean save(String prefix, String id, final Map snapshot, final int maxInactiveInterval) {
      final String sessionId = prefix + ":" + id;

      try {
         this.executor.execute(new JedisCallback() {
            public Void execute(Jedis jedis) {
               if(snapshot.isEmpty()) {
                  jedis.del(sessionId);
               } else {
                  jedis.setex(sessionId, maxInactiveInterval, SessionRedisSource.this.serializer.serialize(snapshot));
               }

               return null;
            }
         }, this.dbIndex);
         return true;
      } catch (Exception var7) {
         log.error("failed to delete session(key={}) in redis,cause:{}", sessionId, Throwables.getStackTraceAsString(var7));
         return false;
      }
   }

   public void destroy() {
      if(this.executor != null) {
         this.executor.getJedisPool().destroy();
      }

   }
}
