package cn.blmdz.wolf.auth.web.aop;

import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;

import cn.blmdz.hunt.common.UserUtil;
import cn.blmdz.wolf.auth.core.Authenticator;
import cn.blmdz.wolf.auth.util.HtmlAuthHelper;
import cn.blmdz.wolf.common.model.ParanaUser;

public class PageRenderAspect {
   private final Authenticator authenticator;

   public PageRenderAspect(Authenticator authenticator) {
      this.authenticator = authenticator;
   }

   @Around("execution(public java.lang.String cn.blmdz.hunt.engine.PageRender.render(..)) && args(domain, path, ..)")
   public Object filterAuth(ProceedingJoinPoint pjp, String domain, String path) throws Throwable {
      String html = (String)pjp.proceed();
      if(!"i18n".equalsIgnoreCase(path)) {
         ParanaUser user = (ParanaUser)UserUtil.getCurrentUser();
         html = HtmlAuthHelper.filter(this.authenticator, user, html);
      }

      return html;
   }
}
