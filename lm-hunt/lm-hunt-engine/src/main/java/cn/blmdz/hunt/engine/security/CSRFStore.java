package cn.blmdz.hunt.engine.security;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Component;

import cn.blmdz.home.common.redis.utils.JedisTemplate;
import cn.blmdz.home.common.redis.utils.JedisTemplate.JedisAction;
import cn.blmdz.home.common.redis.utils.JedisTemplate.JedisActionNoResult;
import redis.clients.jedis.Jedis;

@Component
public class CSRFStore {
   private static final Logger log = LoggerFactory.getLogger(CSRFStore.class);
   @Autowired(
      required = false
   )
   @Qualifier("pampasJedisTemplate")
   private JedisTemplate jedisTemplate;
   private static final int EXPIRE_TIME = 300;

   public boolean checkAndRemoveToken(final String unid, final String token) {
      this.checkJedisExist();
      return ((Boolean)this.jedisTemplate.execute(new JedisAction() {
         public Boolean action(Jedis jedis) {
            return Boolean.valueOf(jedis.srem(CSRFStore.this.getKeyByUnid(unid), new String[]{token}).longValue() > 0L);
         }
      })).booleanValue();
   }

   public void addToken(final String unid, final String... tokens) {
      this.checkJedisExist();
      this.jedisTemplate.execute(new JedisActionNoResult() {
         public void action(Jedis jedis) {
            jedis.sadd(CSRFStore.this.getKeyByUnid(unid), tokens);
            jedis.expire(CSRFStore.this.getKeyByUnid(unid), 300);
         }
      });
   }

   private String getKeyByUnid(String unid) {
      return "pampas:csrf-tokens:" + unid;
   }

   private void checkJedisExist() {
      if(this.jedisTemplate == null) {
         log.error("Need a JedisTemplate to use CSRF token. Please config [pampas.redis].");
         throw new IllegalStateException("Need a JedisTemplate to use CSRF token. Please config [pampas.redis].");
      }
   }
}
