package cn.blmdz.home.session;

import cn.blmdz.home.session.AFSession;
import cn.blmdz.home.session.AFSessionManager;
import cn.blmdz.home.session.util.WebUtil;

import com.google.common.base.Joiner;
import com.google.common.base.Splitter;
import com.google.common.base.Strings;

import java.util.List;

import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletRequestWrapper;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class AFHttpServletRequest extends HttpServletRequestWrapper {
   private static final Logger log = LoggerFactory.getLogger(AFHttpServletRequest.class);
   private final HttpServletRequest request;
   private final HttpServletResponse response;
   private String sessionCookieName;
   private String cookieDomain;
   private String cookieContextPath;
   private int maxInactiveInterval;
   private int cookieMaxAge;
   private String sessionKeyPrefix;
   private AFSessionManager sessionManager;
   private AFSession afSession;

   public AFHttpServletRequest(HttpServletRequest request, HttpServletResponse response) {
      super(request);
      this.request = request;
      this.response = response;
      this.sessionManager = AFSessionManager.getInstance();
   }

   public HttpSession getSession(boolean create) {
      return this.doGetSession(create);
   }

   public HttpSession getSession() {
      return this.doGetSession(true);
   }

   public AFSession currentSession() {
      return this.afSession;
   }

   public String getCookieDomain() {
      if(!Strings.isNullOrEmpty(this.cookieDomain)) {
         return this.cookieDomain;
      } else {
         List<String> parts = Splitter.on('.').omitEmptyStrings().trimResults().splitToList(this.request.getServerName());
         return parts.size() > 2?Joiner.on('.').join(parts.subList(1, parts.size())):this.request.getServerName();
      }
   }

   private HttpSession doGetSession(boolean create) {
      if(this.afSession == null) {
         Cookie cookie = WebUtil.findCookie(this, this.getSessionCookieName());
         if(cookie != null && !"__DELETED__".equals(cookie.getValue())) {
            String value = cookie.getValue();
            log.debug("Find session`s id from cookie.[{}]", value);
            this.afSession = this.buildAFSession(this.sessionKeyPrefix, value, false);
         } else {
            this.afSession = this.buildAFSession(create);
         }
      } else {
         log.debug("Session[{}] was existed.", this.afSession.getId());
      }

      return this.afSession;
   }

   private AFSession buildAFSession(String prefix, String sessionId, boolean cookie) {
      AFSession session = new AFSession(this.sessionManager, this.request, prefix, sessionId);
      session.setMaxInactiveInterval(this.maxInactiveInterval);
      if(cookie) {
         WebUtil.addCookie(this, this.response, this.getSessionCookieName(), sessionId, this.getCookieDomain(), this.getCookieContextPath(), this.cookieMaxAge, true);
      }

      return session;
   }

   private AFSession buildAFSession(boolean create) {
      if(create) {
         this.afSession = this.buildAFSession(this.sessionKeyPrefix, this.sessionManager.getSessionIdGenerator().generateId(this.request), true);
         log.debug("Build new session[{}].", this.afSession.getId());
         return this.afSession;
      } else {
         return null;
      }
   }

   public String getSessionCookieName() {
      return this.sessionCookieName;
   }

   public void setSessionCookieName(String sessionCookieName) {
      this.sessionCookieName = sessionCookieName;
   }

   public void setCookieDomain(String cookieDomain) {
      this.cookieDomain = cookieDomain;
   }

   public String getCookieContextPath() {
      return this.cookieContextPath;
   }

   public void setCookieContextPath(String cookieContextPath) {
      this.cookieContextPath = cookieContextPath;
   }

   public int getMaxInactiveInterval() {
      return this.maxInactiveInterval;
   }

   public void setMaxInactiveInterval(int maxInactiveInterval) {
      this.maxInactiveInterval = maxInactiveInterval;
   }

   public int getCookieMaxAge() {
      return this.cookieMaxAge;
   }

   public void setCookieMaxAge(int cookieMaxAge) {
      this.cookieMaxAge = cookieMaxAge;
   }

   public String getSessionKeyPrefix() {
      return this.sessionKeyPrefix;
   }

   public void setSessionKeyPrefix(String sessionKeyPrefix) {
      this.sessionKeyPrefix = sessionKeyPrefix;
   }

   public AFSessionManager getSessionManager() {
      return this.sessionManager;
   }

   public void setSessionManager(AFSessionManager sessionManager) {
      this.sessionManager = sessionManager;
   }

   public AFSession getAfSession() {
      return this.afSession;
   }

   public void setAfSession(AFSession afSession) {
      this.afSession = afSession;
   }
}
