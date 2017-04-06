package cn.blmdz.wolf.auth.web.interceptor;

import java.io.IOException;
import java.net.URI;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.dozer.DozerBeanMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.stereotype.Component;
import org.springframework.web.servlet.handler.HandlerInterceptorAdapter;
import org.springframework.web.util.UriComponents;
import org.springframework.web.util.UriComponentsBuilder;

import com.google.common.base.Objects;
import com.google.common.base.Strings;

import cn.blmdz.home.common.exception.JsonResponseException;
import cn.blmdz.hunt.common.UserUtil;
import cn.blmdz.hunt.engine.ThreadVars;
import cn.blmdz.wolf.auth.core.Authenticator;
import cn.blmdz.wolf.auth.model.App;
import cn.blmdz.wolf.auth.model.ParanaThreadVars;
import cn.blmdz.wolf.auth.model.Req;
import cn.blmdz.wolf.common.model.ParanaUser;

@ConditionalOnClass({ThreadVars.class})
@Component
public class AuthInterceptor extends HandlerInterceptorAdapter {
   private static final Logger log = LoggerFactory.getLogger(AuthInterceptor.class);
   private DozerBeanMapper mapper = new DozerBeanMapper();
   private final Authenticator authenticator;

   @Autowired
   public AuthInterceptor(Authenticator authenticator) {
      this.authenticator = authenticator;
   }

   public boolean preHandle(HttpServletRequest request, HttpServletResponse response, Object handler) throws Exception {
      this.initParanaThreadVars();
      String path = request.getRequestURI().substring(request.getContextPath().length());
      String method = request.getMethod().toUpperCase();
      Map<String, String[]> params = request.getParameterMap();
      Req req = new Req(path, method, params);
      ParanaUser user = (ParanaUser)UserUtil.getCurrentUser();
      return user == null?this.notLoginCase(request, response, req):this.loginCase(request, response, req, user);
   }

   public void afterCompletion(HttpServletRequest request, HttpServletResponse response, Object handler, Exception ex) throws Exception {
      ParanaThreadVars.clearDomain();
      ParanaThreadVars.clearApp();
   }

   private boolean notLoginCase(HttpServletRequest request, HttpServletResponse response, Req req) throws IOException {
      return this.authenticator.ask((ParanaUser)null, (Req)req) || this.error401(request, response);
   }

   private boolean loginCase(HttpServletRequest request, HttpServletResponse response, Req req, ParanaUser user) {
      return this.authenticator.ask(user, req) || this.error403(request, response);
   }

   private boolean error401(HttpServletRequest request, HttpServletResponse response) throws IOException {
      log.warn("user not login for request: {}", request.getRequestURI());
      if(this.isAjaxRequest(request)) {
         throw new JsonResponseException(401, "user.not.login");
      } else {
         return this.redirectToLogin(request, response);
      }
   }

   private boolean error403(HttpServletRequest request, HttpServletResponse response) {
      log.warn("user no permission for request: {}", request.getRequestURI());
      throw new JsonResponseException(403, "user.no.permission");
   }

   private boolean redirectToLogin(HttpServletRequest request, HttpServletResponse response) throws IOException {
      String currentUrl = this.getCurrentURL(request);
      UriComponents uriComponents = UriComponentsBuilder.fromUriString("http://" + ParanaThreadVars.getHost() + "/login?target={target}").build();
      URI uri = uriComponents.expand(new Object[]{currentUrl}).encode().toUri();
      response.sendRedirect(uri.toString());
      return false;
   }

   private String getCurrentURL(HttpServletRequest request) {
      StringBuilder builder = new StringBuilder();
      builder.append(request.getScheme()).append("://").append(request.getServerName());
      int port = request.getServerPort();
      if(port != 80 && port != 443) {
         builder.append(":").append(port);
      }

      String path = request.getRequestURI().substring(request.getContextPath().length());
      builder.append(path);
      String qs = request.getQueryString();
      if(!Strings.isNullOrEmpty(qs)) {
         builder.append("?").append(qs);
      }

      return builder.toString();
   }

   private boolean isAjaxRequest(HttpServletRequest request) {
      return Objects.equal(request.getHeader("X-Requested-With"), "XMLHttpRequest");
   }

   private void initParanaThreadVars() {
      ParanaThreadVars.setDomain(ThreadVars.getDomain());
      ParanaThreadVars.setHost(ThreadVars.getHost());
      App app = (App)this.mapper.map(ThreadVars.getApp(), App.class);
      ParanaThreadVars.setApp(app);
      ParanaThreadVars.setLocale(ThreadVars.getLocale());
   }
}
