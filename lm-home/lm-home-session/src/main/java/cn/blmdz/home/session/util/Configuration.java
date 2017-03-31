package cn.blmdz.home.session.util;

public class Configuration {
   private String source = "redis";
   private Boolean sessionRedisCluster = Boolean.FALSE;
   private String sessionRedisPrefix = "afsession";
   private Boolean sessionRedisTestOnBorrow = Boolean.TRUE;
   private Integer sessionRedisMaxIdle = Integer.valueOf(2);
   private Integer sessionRedisMaxTotal = Integer.valueOf(5);
   private String serializeType = "json";
   private String sessionRedisHost;
   private Integer sessionRedisPort;
   private Integer sessionRedisDbIndex = Integer.valueOf(0);
   private String sessionRedisSentinelHosts;
   private String sessionRedisSentinelMasterName;

   public Boolean isCluster() {
      return this.sessionRedisCluster;
   }

   public String getSource() {
      return this.source;
   }

   public Boolean getSessionRedisCluster() {
      return this.sessionRedisCluster;
   }

   public String getSessionRedisPrefix() {
      return this.sessionRedisPrefix;
   }

   public Boolean getSessionRedisTestOnBorrow() {
      return this.sessionRedisTestOnBorrow;
   }

   public Integer getSessionRedisMaxIdle() {
      return this.sessionRedisMaxIdle;
   }

   public Integer getSessionRedisMaxTotal() {
      return this.sessionRedisMaxTotal;
   }

   public String getSerializeType() {
      return this.serializeType;
   }

   public String getSessionRedisHost() {
      return this.sessionRedisHost;
   }

   public Integer getSessionRedisPort() {
      return this.sessionRedisPort;
   }

   public Integer getSessionRedisDbIndex() {
      return this.sessionRedisDbIndex;
   }

   public String getSessionRedisSentinelHosts() {
      return this.sessionRedisSentinelHosts;
   }

   public String getSessionRedisSentinelMasterName() {
      return this.sessionRedisSentinelMasterName;
   }

   public void setSource(String source) {
      this.source = source;
   }

   public void setSessionRedisCluster(Boolean sessionRedisCluster) {
      this.sessionRedisCluster = sessionRedisCluster;
   }

   public void setSessionRedisPrefix(String sessionRedisPrefix) {
      this.sessionRedisPrefix = sessionRedisPrefix;
   }

   public void setSessionRedisTestOnBorrow(Boolean sessionRedisTestOnBorrow) {
      this.sessionRedisTestOnBorrow = sessionRedisTestOnBorrow;
   }

   public void setSessionRedisMaxIdle(Integer sessionRedisMaxIdle) {
      this.sessionRedisMaxIdle = sessionRedisMaxIdle;
   }

   public void setSessionRedisMaxTotal(Integer sessionRedisMaxTotal) {
      this.sessionRedisMaxTotal = sessionRedisMaxTotal;
   }

   public void setSerializeType(String serializeType) {
      this.serializeType = serializeType;
   }

   public void setSessionRedisHost(String sessionRedisHost) {
      this.sessionRedisHost = sessionRedisHost;
   }

   public void setSessionRedisPort(Integer sessionRedisPort) {
      this.sessionRedisPort = sessionRedisPort;
   }

   public void setSessionRedisDbIndex(Integer sessionRedisDbIndex) {
      this.sessionRedisDbIndex = sessionRedisDbIndex;
   }

   public void setSessionRedisSentinelHosts(String sessionRedisSentinelHosts) {
      this.sessionRedisSentinelHosts = sessionRedisSentinelHosts;
   }

   public void setSessionRedisSentinelMasterName(String sessionRedisSentinelMasterName) {
      this.sessionRedisSentinelMasterName = sessionRedisSentinelMasterName;
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof Configuration)) {
         return false;
      } else {
         Configuration other = (Configuration)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$source = this.getSource();
            Object other$source = other.getSource();
            if(this$source == null) {
               if(other$source != null) {
                  return false;
               }
            } else if(!this$source.equals(other$source)) {
               return false;
            }

            Object this$sessionRedisCluster = this.getSessionRedisCluster();
            Object other$sessionRedisCluster = other.getSessionRedisCluster();
            if(this$sessionRedisCluster == null) {
               if(other$sessionRedisCluster != null) {
                  return false;
               }
            } else if(!this$sessionRedisCluster.equals(other$sessionRedisCluster)) {
               return false;
            }

            Object this$sessionRedisPrefix = this.getSessionRedisPrefix();
            Object other$sessionRedisPrefix = other.getSessionRedisPrefix();
            if(this$sessionRedisPrefix == null) {
               if(other$sessionRedisPrefix != null) {
                  return false;
               }
            } else if(!this$sessionRedisPrefix.equals(other$sessionRedisPrefix)) {
               return false;
            }

            Object this$sessionRedisTestOnBorrow = this.getSessionRedisTestOnBorrow();
            Object other$sessionRedisTestOnBorrow = other.getSessionRedisTestOnBorrow();
            if(this$sessionRedisTestOnBorrow == null) {
               if(other$sessionRedisTestOnBorrow != null) {
                  return false;
               }
            } else if(!this$sessionRedisTestOnBorrow.equals(other$sessionRedisTestOnBorrow)) {
               return false;
            }

            Object this$sessionRedisMaxIdle = this.getSessionRedisMaxIdle();
            Object other$sessionRedisMaxIdle = other.getSessionRedisMaxIdle();
            if(this$sessionRedisMaxIdle == null) {
               if(other$sessionRedisMaxIdle != null) {
                  return false;
               }
            } else if(!this$sessionRedisMaxIdle.equals(other$sessionRedisMaxIdle)) {
               return false;
            }

            Object this$sessionRedisMaxTotal = this.getSessionRedisMaxTotal();
            Object other$sessionRedisMaxTotal = other.getSessionRedisMaxTotal();
            if(this$sessionRedisMaxTotal == null) {
               if(other$sessionRedisMaxTotal != null) {
                  return false;
               }
            } else if(!this$sessionRedisMaxTotal.equals(other$sessionRedisMaxTotal)) {
               return false;
            }

            Object this$serializeType = this.getSerializeType();
            Object other$serializeType = other.getSerializeType();
            if(this$serializeType == null) {
               if(other$serializeType != null) {
                  return false;
               }
            } else if(!this$serializeType.equals(other$serializeType)) {
               return false;
            }

            Object this$sessionRedisHost = this.getSessionRedisHost();
            Object other$sessionRedisHost = other.getSessionRedisHost();
            if(this$sessionRedisHost == null) {
               if(other$sessionRedisHost != null) {
                  return false;
               }
            } else if(!this$sessionRedisHost.equals(other$sessionRedisHost)) {
               return false;
            }

            Object this$sessionRedisPort = this.getSessionRedisPort();
            Object other$sessionRedisPort = other.getSessionRedisPort();
            if(this$sessionRedisPort == null) {
               if(other$sessionRedisPort != null) {
                  return false;
               }
            } else if(!this$sessionRedisPort.equals(other$sessionRedisPort)) {
               return false;
            }

            Object this$sessionRedisDbIndex = this.getSessionRedisDbIndex();
            Object other$sessionRedisDbIndex = other.getSessionRedisDbIndex();
            if(this$sessionRedisDbIndex == null) {
               if(other$sessionRedisDbIndex != null) {
                  return false;
               }
            } else if(!this$sessionRedisDbIndex.equals(other$sessionRedisDbIndex)) {
               return false;
            }

            Object this$sessionRedisSentinelHosts = this.getSessionRedisSentinelHosts();
            Object other$sessionRedisSentinelHosts = other.getSessionRedisSentinelHosts();
            if(this$sessionRedisSentinelHosts == null) {
               if(other$sessionRedisSentinelHosts != null) {
                  return false;
               }
            } else if(!this$sessionRedisSentinelHosts.equals(other$sessionRedisSentinelHosts)) {
               return false;
            }

            Object this$sessionRedisSentinelMasterName = this.getSessionRedisSentinelMasterName();
            Object other$sessionRedisSentinelMasterName = other.getSessionRedisSentinelMasterName();
            if(this$sessionRedisSentinelMasterName == null) {
               if(other$sessionRedisSentinelMasterName != null) {
                  return false;
               }
            } else if(!this$sessionRedisSentinelMasterName.equals(other$sessionRedisSentinelMasterName)) {
               return false;
            }

            return true;
         }
      }
   }

   public boolean canEqual(Object other) {
      return other instanceof Configuration;
   }

   public int hashCode() {
      int PRIME = 31;
      int result = 1;
      Object $source = this.getSource();
      result = result * 31 + ($source == null?0:$source.hashCode());
      Object $sessionRedisCluster = this.getSessionRedisCluster();
      result = result * 31 + ($sessionRedisCluster == null?0:$sessionRedisCluster.hashCode());
      Object $sessionRedisPrefix = this.getSessionRedisPrefix();
      result = result * 31 + ($sessionRedisPrefix == null?0:$sessionRedisPrefix.hashCode());
      Object $sessionRedisTestOnBorrow = this.getSessionRedisTestOnBorrow();
      result = result * 31 + ($sessionRedisTestOnBorrow == null?0:$sessionRedisTestOnBorrow.hashCode());
      Object $sessionRedisMaxIdle = this.getSessionRedisMaxIdle();
      result = result * 31 + ($sessionRedisMaxIdle == null?0:$sessionRedisMaxIdle.hashCode());
      Object $sessionRedisMaxTotal = this.getSessionRedisMaxTotal();
      result = result * 31 + ($sessionRedisMaxTotal == null?0:$sessionRedisMaxTotal.hashCode());
      Object $serializeType = this.getSerializeType();
      result = result * 31 + ($serializeType == null?0:$serializeType.hashCode());
      Object $sessionRedisHost = this.getSessionRedisHost();
      result = result * 31 + ($sessionRedisHost == null?0:$sessionRedisHost.hashCode());
      Object $sessionRedisPort = this.getSessionRedisPort();
      result = result * 31 + ($sessionRedisPort == null?0:$sessionRedisPort.hashCode());
      Object $sessionRedisDbIndex = this.getSessionRedisDbIndex();
      result = result * 31 + ($sessionRedisDbIndex == null?0:$sessionRedisDbIndex.hashCode());
      Object $sessionRedisSentinelHosts = this.getSessionRedisSentinelHosts();
      result = result * 31 + ($sessionRedisSentinelHosts == null?0:$sessionRedisSentinelHosts.hashCode());
      Object $sessionRedisSentinelMasterName = this.getSessionRedisSentinelMasterName();
      result = result * 31 + ($sessionRedisSentinelMasterName == null?0:$sessionRedisSentinelMasterName.hashCode());
      return result;
   }

   public String toString() {
      return "Configuration(source=" + this.getSource() + ", sessionRedisCluster=" + this.getSessionRedisCluster() + ", sessionRedisPrefix=" + this.getSessionRedisPrefix() + ", sessionRedisTestOnBorrow=" + this.getSessionRedisTestOnBorrow() + ", sessionRedisMaxIdle=" + this.getSessionRedisMaxIdle() + ", sessionRedisMaxTotal=" + this.getSessionRedisMaxTotal() + ", serializeType=" + this.getSerializeType() + ", sessionRedisHost=" + this.getSessionRedisHost() + ", sessionRedisPort=" + this.getSessionRedisPort() + ", sessionRedisDbIndex=" + this.getSessionRedisDbIndex() + ", sessionRedisSentinelHosts=" + this.getSessionRedisSentinelHosts() + ", sessionRedisSentinelMasterName=" + this.getSessionRedisSentinelMasterName() + ")";
   }
}
