package cn.blmdz.boot.hunt.autoconfigure;

import org.springframework.boot.context.properties.ConfigurationProperties;

import com.google.common.base.MoreObjects;

import cn.blmdz.hunt.engine.AbstractSetting;

@ConfigurationProperties(
   prefix = "pampas"
)
public class PampasProperties extends AbstractSetting {
   private PampasProperties.RedisProperties redis;

   public String toString() {
      return "PampasProperties{redis=" + this.redis + "} " + super.toString();
   }

   public PampasProperties.RedisProperties getRedis() {
      return this.redis;
   }

   public void setRedis(PampasProperties.RedisProperties redis) {
      this.redis = redis;
   }

   public static class RedisProperties {
      private String jedisPool;
      private String host;
      private Integer port;
      private Integer timeout;
      private String password;
      private Integer database;

      public String toString() {
         return MoreObjects.toStringHelper(this).add("database", this.database).add("jedisPool", this.jedisPool).add("host", this.host).add("port", this.port).add("timeout", this.timeout).add("password", this.password).toString();
      }

      public String getJedisPool() {
         return this.jedisPool;
      }

      public void setJedisPool(String jedisPool) {
         this.jedisPool = jedisPool;
      }

      public String getHost() {
         return this.host;
      }

      public void setHost(String host) {
         this.host = host;
      }

      public Integer getPort() {
         return this.port;
      }

      public void setPort(Integer port) {
         this.port = port;
      }

      public Integer getTimeout() {
         return this.timeout;
      }

      public void setTimeout(Integer timeout) {
         this.timeout = timeout;
      }

      public String getPassword() {
         return this.password;
      }

      public void setPassword(String password) {
         this.password = password;
      }

      public Integer getDatabase() {
         return this.database;
      }

      public void setDatabase(Integer database) {
         this.database = database;
      }
   }
}
