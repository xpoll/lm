package cn.blmdz.home.session;

import cn.blmdz.home.session.AFHttpServletRequest;
import cn.blmdz.home.session.AFSession;
import cn.blmdz.home.session.AFSessionManager;
import cn.blmdz.home.session.util.Configuration;
import cn.blmdz.home.session.util.StringUtil;
import cn.blmdz.home.session.util.WebUtil;

import com.google.common.base.MoreObjects;
import com.google.common.io.Resources;

import java.io.IOException;
import java.io.InputStream;
import java.util.Map;
import java.util.Properties;

import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class AFSessionFilter implements Filter {
   private static final Logger log = LoggerFactory.getLogger(AFSessionFilter.class);
   private AFSessionManager sessionManager;
   private String sessionCookieName;
   private int maxInactiveInterval;
   private String cookieDomain;
   private String cookieContextPath;
   private int cookieMaxAge;
   private String sessionKeyPrefix;

   public AFSessionFilter(boolean withSpringBoot) {
      log.info("from spring boot starter");
   }

   public AFSessionFilter() {
      Properties properties = new Properties();
      InputStream input = null;

      try {
         input = Resources.asByteSource(Resources.getResource("session.properties")).openStream();
         properties.load(input);
      } catch (Exception var22) {
         log.error("failed to load session.properties", var22);
      } finally {
         if(input != null) {
            try {
               input.close();
            } catch (IOException var21) {
               ;
            }
         }

      }

      String sessionSource = properties.getProperty("session.source");
      Configuration configuration = new Configuration();
      if(!StringUtil.isEmpty(sessionSource) && !StringUtil.startsWithIgnoreCase(sessionSource, "$")) {
         log.info("AFSessionFilter set configuration[session.source] -> {}", sessionSource);
         configuration.setSource(sessionSource);
      }

      String sessionRedisSerializeType = properties.getProperty("session.serialize.type");
      if(!StringUtil.isEmpty(sessionRedisSerializeType) && !StringUtil.startsWithIgnoreCase(sessionRedisSerializeType, "$")) {
         log.info("AFSessionFilter set configuration[session.serialize.type] -> {}", sessionRedisSerializeType);
         configuration.setSerializeType(sessionRedisSerializeType);
      }

      String sessionKeyPrefix = properties.getProperty("session.redis.prefix");
      if(!StringUtil.isEmpty(sessionKeyPrefix) && !StringUtil.startsWithIgnoreCase(sessionKeyPrefix, "$")) {
         log.info("AFSessionFilter set configuration[session.redis.prefix] -> {}", sessionKeyPrefix);
         configuration.setSessionRedisPrefix(sessionKeyPrefix);
         this.sessionKeyPrefix = sessionKeyPrefix;
      }

      String sessionRedisCluster = properties.getProperty("session.redis.cluster");
      if(!StringUtil.isEmpty(sessionRedisCluster) && !StringUtil.startsWithIgnoreCase(sessionRedisCluster, "$")) {
         log.info("AFSessionFilter set configuration[session.redis.cluster] -> {}", sessionRedisCluster);
         configuration.setSessionRedisCluster(Boolean.valueOf(Boolean.parseBoolean(sessionRedisCluster)));
      }

      String sessionRedisTestOnBorrow = properties.getProperty("session.redis.test.on.borrow");
      if(!StringUtil.isEmpty(sessionRedisTestOnBorrow) && !StringUtil.startsWithIgnoreCase(sessionRedisTestOnBorrow, "$")) {
         log.info("AFSessionFilter set configuration[session.redis.test.on.borrow] -> {}", sessionRedisTestOnBorrow);
         configuration.setSessionRedisTestOnBorrow(Boolean.valueOf(Boolean.parseBoolean(sessionRedisTestOnBorrow)));
      }

      String sessionRedisMaxIdle = properties.getProperty("session.redis.max.idle");
      if(!StringUtil.isEmpty(sessionRedisMaxIdle) && !StringUtil.startsWithIgnoreCase(sessionRedisMaxIdle, "$")) {
         log.info("AFSessionFilter set configuration[session.redis.max.idle] -> {}", sessionRedisMaxIdle);
         configuration.setSessionRedisMaxIdle(Integer.valueOf(Integer.parseInt(sessionRedisMaxIdle)));
      }

      String sessionRedisMaxTotal = properties.getProperty("session.redis.max.total");
      if(!StringUtil.isEmpty(sessionRedisMaxTotal) && !StringUtil.startsWithIgnoreCase(sessionRedisMaxTotal, "$")) {
         log.info("AFSessionFilter set configuration[session.redis.max.total] -> {}", sessionRedisMaxTotal);
         configuration.setSessionRedisMaxTotal(Integer.valueOf(Integer.parseInt(sessionRedisMaxTotal)));
      }

      String sessionRedisHost = properties.getProperty("session.redis.host");
      if(!StringUtil.isEmpty(sessionRedisHost) && !StringUtil.startsWithIgnoreCase(sessionRedisHost, "$")) {
         log.info("AFSessionFilter set configuration[session.redis.host] -> {}", sessionRedisHost);
         configuration.setSessionRedisHost(sessionRedisHost);
      }

      String sessionRedisPort = properties.getProperty("session.redis.port");
      if(!StringUtil.isEmpty(sessionRedisPort) && !StringUtil.startsWithIgnoreCase(sessionRedisPort, "$")) {
         log.info("AFSessionFilter set configuration[session.redis.port] -> {}", sessionRedisPort);
         configuration.setSessionRedisPort(Integer.valueOf(Integer.parseInt(sessionRedisPort)));
      }

      String sessionRedisSentinelHosts = properties.getProperty("session.redis.sentinel.hosts");
      if(!StringUtil.isEmpty(sessionRedisSentinelHosts) && !StringUtil.startsWithIgnoreCase(sessionRedisSentinelHosts, "$")) {
         log.info("AFSessionFilter set configuration[session.redis.sentinel.hosts] -> {}", sessionRedisSentinelHosts);
         configuration.setSessionRedisSentinelHosts(sessionRedisSentinelHosts);
      }

      String sessionRedisSentinelMasterName = properties.getProperty("session.redis.sentinel.master.name");
      if(!StringUtil.isEmpty(sessionRedisSentinelMasterName) && !StringUtil.startsWithIgnoreCase(sessionRedisSentinelMasterName, "$")) {
         log.info("AFSessionFilter set configuration[session.redis.sentinel.master.name] -> {}", sessionRedisSentinelMasterName);
         configuration.setSessionRedisSentinelMasterName(sessionRedisSentinelMasterName);
      }

      String sessionRedisDbIndex = properties.getProperty("session.redis.db.index");
      if(!StringUtil.isEmpty(sessionRedisDbIndex) && !StringUtil.startsWithIgnoreCase(sessionRedisDbIndex, "$")) {
         log.info("AFSessionFilter set configuration[session.redis.db.index] -> {}", sessionRedisDbIndex);
         configuration.setSessionRedisDbIndex(Integer.valueOf(Integer.parseInt(sessionRedisDbIndex)));
      }

      this.sessionManager = AFSessionManager.newInstance(configuration);
   }

   public void init(FilterConfig filterConfig) throws ServletException {
      try {
         this.initParameters(filterConfig);
      } catch (Exception var3) {
         log.error("failed to init cache session filter", var3);
         throw new ServletException(var3);
      }
   }

   public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain) throws IOException, ServletException {
      if(request instanceof AFHttpServletRequest) {
         chain.doFilter(request, response);
      } else {
         HttpServletRequest httpRequest = (HttpServletRequest)request;
         HttpServletResponse httpResponse = (HttpServletResponse)response;
         AFHttpServletRequest afRequest = new AFHttpServletRequest(httpRequest, httpResponse);
         afRequest.setSessionCookieName(this.sessionCookieName);
         afRequest.setMaxInactiveInterval(this.maxInactiveInterval);
         afRequest.setCookieDomain(this.cookieDomain);
         afRequest.setCookieContextPath(this.cookieContextPath);
         afRequest.setCookieMaxAge(this.cookieMaxAge);
         afRequest.setSessionKeyPrefix(this.sessionKeyPrefix);
         chain.doFilter(afRequest, response);
         AFSession session = afRequest.currentSession();
         if(session != null) {
            if(!session.isValid()) {
               log.debug("delete login cookie");
               WebUtil.failureCookie(httpRequest, httpResponse, this.sessionCookieName, this.cookieDomain, this.cookieContextPath);
            } else if(session.isDirty()) {
               if(log.isDebugEnabled()) {
                  log.debug("try to flush session to session store");
               }

               Map<String, Object> snapshot = session.snapshot();
               if(this.sessionManager.save(this.sessionKeyPrefix, session.getId(), snapshot, this.maxInactiveInterval)) {
                  if(log.isDebugEnabled()) {
                     log.debug("succeed to flush session {} to store, key is:{}", snapshot, session.getId());
                  }
               } else {
                  log.error("failed to save session to redis");
                  WebUtil.failureCookie(httpRequest, httpResponse, this.sessionCookieName, this.cookieDomain, this.cookieContextPath);
               }
            } else {
               this.sessionManager.refreshExpireTime(session, this.maxInactiveInterval);
            }
         }

      }
   }

   public void destroy() {
      this.sessionManager.destroy();
   }

   private void initParameters(FilterConfig filterConfig) throws ClassNotFoundException, InstantiationException, IllegalAccessException {
      String sessionCookieNameParameter = "sessionCookieName";
      String maxInactiveIntervalParameter = "maxInactiveInterval";
      String cookieDomainParameter = "cookieDomain";
      String cookieContextPathParameter = "cookieContextPath";
      String temp = filterConfig.getInitParameter(sessionCookieNameParameter);
      this.sessionCookieName = temp == null?"afsid":temp;
      temp = filterConfig.getInitParameter(maxInactiveIntervalParameter);
      this.maxInactiveInterval = temp == null?1800:Integer.parseInt(temp);
      if(filterConfig.getInitParameter(cookieDomainParameter) != null) {
         this.cookieDomain = filterConfig.getInitParameter(cookieDomainParameter);
      }

      temp = filterConfig.getInitParameter(cookieContextPathParameter);
      this.cookieContextPath = temp == null?"/":temp;
      this.cookieMaxAge = Integer.parseInt((String)MoreObjects.firstNonNull(filterConfig.getInitParameter("cookieMaxAge"), "-1"));
      log.info("CacheSessionFilter (sessionCookieName={},maxInactiveInterval={},cookieDomain={})", new Object[]{this.sessionCookieName, Integer.valueOf(this.maxInactiveInterval), this.cookieDomain});
   }

   public void setSessionManager(AFSessionManager sessionManager) {
      this.sessionManager = sessionManager;
   }

   public void setSessionCookieName(String sessionCookieName) {
      this.sessionCookieName = sessionCookieName;
   }

   public void setMaxInactiveInterval(int maxInactiveInterval) {
      this.maxInactiveInterval = maxInactiveInterval;
   }

   public void setCookieDomain(String cookieDomain) {
      this.cookieDomain = cookieDomain;
   }

   public void setCookieContextPath(String cookieContextPath) {
      this.cookieContextPath = cookieContextPath;
   }

   public void setCookieMaxAge(int cookieMaxAge) {
      this.cookieMaxAge = cookieMaxAge;
   }

   public void setSessionKeyPrefix(String sessionKeyPrefix) {
      this.sessionKeyPrefix = sessionKeyPrefix;
   }
}
