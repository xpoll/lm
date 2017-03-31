package cn.blmdz.hunt.engine.security;

import javax.servlet.http.HttpServletRequest;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.google.common.base.Strings;

@Component
public class CSRFHelper {
   private static final Logger log = LoggerFactory.getLogger(CSRFHelper.class);
   @Autowired
   private CSRFStore csrfStore;
   public static final String CSRF_TOKEN_KEY_IN_REQUEST = "_csrf_token_";

   public boolean check(HttpServletRequest request) {
      String tokenInRequest = request.getParameter("_csrf_token_");
      if(Strings.isNullOrEmpty(tokenInRequest)) {
         log.warn("csrf check needed, but no token found in request, ignore this request {}", request.getContextPath());
         return false;
      } else {
         String sessionId = request.getSession().getId();
         if(!this.csrfStore.checkAndRemoveToken(sessionId, tokenInRequest)) {
            log.warn("csrf check needed, but token in request [{}] not match, sessionId: [{}]", tokenInRequest, sessionId);
            return false;
         } else {
            log.debug("csrf check passed, token in request [{}], sessionId: [{}]", tokenInRequest, sessionId);
            return true;
         }
      }
   }

   public void save(HttpServletRequest request, String[] tokens) {
      String sessionId = request.getSession().getId();
      this.csrfStore.addToken(sessionId, tokens);
   }
}
