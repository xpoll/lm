package io.terminus.parana.auth.web.aop;

import io.terminus.pampas.common.UserUtil;
import io.terminus.parana.auth.core.Authenticator;
import io.terminus.parana.auth.util.HtmlAuthHelper;
import io.terminus.parana.common.model.ParanaUser;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;

public class PageRenderAspect {
   private final Authenticator authenticator;

   public PageRenderAspect(Authenticator authenticator) {
      this.authenticator = authenticator;
   }

   @Around("execution(public java.lang.String io.terminus.pampas.engine.PageRender.render(..)) && args(domain, path, ..)")
   public Object filterAuth(ProceedingJoinPoint pjp, String domain, String path) throws Throwable {
      String html = (String)pjp.proceed();
      if(!"i18n".equalsIgnoreCase(path)) {
         ParanaUser user = (ParanaUser)UserUtil.getCurrentUser();
         html = HtmlAuthHelper.filter(this.authenticator, user, html);
      }

      return html;
   }
}
