package cn.blmdz.wolf.web.core.interceptors;

import java.io.IOException;
import java.net.URI;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.regex.Pattern;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpMethod;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;
import org.springframework.web.servlet.handler.HandlerInterceptorAdapter;
import org.springframework.web.util.UriComponents;
import org.springframework.web.util.UriComponentsBuilder;
import org.yaml.snakeyaml.Yaml;

import com.google.common.base.Charsets;
import com.google.common.base.Objects;
import com.google.common.base.Preconditions;
import com.google.common.base.Splitter;
import com.google.common.base.Strings;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.ImmutableSet.Builder;
import com.google.common.collect.Sets;
import com.google.common.io.LineProcessor;
import com.google.common.io.Resources;

import cn.blmdz.home.common.model.BaseUser;
import cn.blmdz.hunt.common.UserUtil;
import cn.blmdz.wolf.web.core.auth.RoleAuthorizor;
import cn.blmdz.wolf.web.core.auth.TypeAuthorizor;
import cn.blmdz.wolf.web.core.exceptions.NotLoginException;
import cn.blmdz.wolf.web.core.exceptions.UnAuthorizationException;

@Component
public class AuthInterceptor extends HandlerInterceptorAdapter {
   private static final Logger log = LoggerFactory.getLogger(AuthInterceptor.class);
   private final Set<WhiteItem> whiteList;
   private final Set<AuthItem> protectList;
   private final TypeAuthorizor typeAuthorizor;
   private final RoleAuthorizor roleAuthorizor;

   @Autowired
   public AuthInterceptor(TypeAuthorizor typeAuthorizor, RoleAuthorizor roleAuthorizor) throws Exception {
      this.typeAuthorizor = typeAuthorizor;
      this.roleAuthorizor = roleAuthorizor;
      this.protectList = Sets.newHashSet();
      Yaml yaml = new Yaml();
      AuthInterceptor.Auths auths = (AuthInterceptor.Auths)yaml.loadAs(Resources.toString(Resources.getResource("auth/auths.yaml"), Charsets.UTF_8), AuthInterceptor.Auths.class);
      if(auths != null) {
         for(AuthInterceptor.Auth auth : auths.auths) {
            Pattern urlPattern = Pattern.compile("^" + auth.url + "$");
            Set<String> types = Sets.newHashSet();
            if(!CollectionUtils.isEmpty(auth.types)) {
               for(String type : auth.types) {
                  types.add(type.toUpperCase());
               }
            }

            Set<String> roles = Sets.newHashSet();
            if(!CollectionUtils.isEmpty(auth.roles)) {
               roles.addAll(auth.roles);
            }

            Set<HttpMethod> methods = Sets.newHashSet();
            if(!CollectionUtils.isEmpty(auth.methods)) {
               for(String method : auth.methods) {
                  methods.add(HttpMethod.valueOf(method.toUpperCase()));
               }
            } else {
               Collections.addAll(methods, HttpMethod.values());
            }

            AuthInterceptor.AuthItem authItem = new AuthInterceptor.AuthItem(urlPattern, types, roles, methods);
            this.protectList.add(authItem);
         }
      }

      this.whiteList = Sets.newHashSet();
      Resources.readLines(Resources.getResource("auth/white_list"), Charsets.UTF_8, new LineProcessor() {
         public boolean processLine(String line) throws IOException {
            if(!Strings.isNullOrEmpty(line)) {
               List<String> parts = Splitter.on(':').trimResults().splitToList(line);
               Preconditions.checkArgument(parts.size() == 2, "illegal white_list configuration [%s]", new Object[]{line});
               Pattern urlPattern = Pattern.compile("^" + (String)parts.get(0) + "$");
               String methods = ((String)parts.get(1)).toUpperCase();
               Builder<HttpMethod> httpMethods = ImmutableSet.builder();

               for(String method : Splitter.on(',').omitEmptyStrings().trimResults().split(methods)) {
                  httpMethods.add(HttpMethod.valueOf(method.toUpperCase()));
               }

               AuthInterceptor.this.whiteList.add(new AuthInterceptor.WhiteItem(urlPattern, httpMethods.build()));
            }

            return true;
         }

         public Void getResult() {
            return null;
         }
      });
   }

   public boolean preHandle(HttpServletRequest request, HttpServletResponse response, Object handler) throws Exception {
      String requestURI = request.getRequestURI().substring(request.getContextPath().length());
      BaseUser user = UserUtil.getCurrentUser();
      HttpMethod method = HttpMethod.valueOf(request.getMethod().toUpperCase());

      for(AuthInterceptor.WhiteItem whiteItem : this.whiteList) {
         if(whiteItem.httpMethods.contains(method) && whiteItem.pattern.matcher(requestURI).matches()) {
            return true;
         }
      }

      boolean inProtectedList = false;

      for(AuthInterceptor.AuthItem authItem : this.protectList) {
         if(authItem.methods.contains(method) && authItem.pattern.matcher(requestURI).matches()) {
            inProtectedList = true;
            if(user == null) {
               this.redirectToLogin(request, response);
               return false;
            }

            if(this.typeAuthorizor.matches(authItem.types, user.getType()) && this.roleAuthorizor.matches(authItem.roles, user.getRoles())) {
               return true;
            }
         }
      }

      if(inProtectedList) {
         throw new UnAuthorizationException();
      } else if((method != HttpMethod.GET || method != HttpMethod.HEAD) && user == null) {
         this.redirectToLogin(request, response);
         return false;
      } else {
         return true;
      }
   }

   private void redirectToLogin(HttpServletRequest request, HttpServletResponse response) throws Exception {
      String currentUrl = request.getRequestURL().toString();
      if(!Strings.isNullOrEmpty(request.getQueryString())) {
         currentUrl = currentUrl + "?" + request.getQueryString();
      }

      UriComponents uriComponents = UriComponentsBuilder.fromUriString("/login?target={target}").build();
      URI uri = uriComponents.expand(new Object[]{currentUrl}).encode().toUri();
      if(this.isAjaxRequest(request)) {
         throw new NotLoginException(uri.toString());
      } else {
         response.sendRedirect(uri.toString());
      }
   }

   private boolean isAjaxRequest(HttpServletRequest request) {
      return Objects.equal(request.getHeader("X-Requested-With"), "XMLHttpRequest");
   }

   private static class Auth {
      public String url;
      public List<String> types;
      public List roles;
      public List<String> methods;

      public String toString() {
         return "AuthInterceptor.Auth(url=" + this.url + ", types=" + this.types + ", roles=" + this.roles + ", methods=" + this.methods + ")";
      }
   }

   public static class AuthItem {
      public final Pattern pattern;
      public final Set types;
      public final Set roles;
      public final Set methods;

      public AuthItem(Pattern pattern, Set types, Set roles, Set methods) {
         this.pattern = pattern;
         this.types = types;
         this.roles = roles;
         this.methods = methods;
      }

      public boolean equals(Object o) {
         if(this == o) {
            return true;
         } else if(o != null && this.getClass() == o.getClass()) {
            AuthInterceptor.AuthItem authItem = (AuthInterceptor.AuthItem)o;
            if(this.pattern != null) {
               if(!this.pattern.equals(authItem.pattern)) {
                  return false;
               }
            } else if(authItem.pattern != null) {
               return false;
            }

            return true;
         } else {
            return false;
         }
      }

      public int hashCode() {
         return this.pattern != null?this.pattern.hashCode():0;
      }
   }

   private static class Auths {
      public List<Auth> auths;

      public String toString() {
         return "AuthInterceptor.Auths(auths=" + this.auths + ")";
      }
   }

   public static class WhiteItem {
      public final Pattern pattern;
      public final Set httpMethods;

      public WhiteItem(Pattern pattern, Set httpMethods) {
         this.pattern = pattern;
         this.httpMethods = httpMethods;
      }
   }
}
