package io.terminus.parana.auth.util;

import com.google.common.io.Resources;
import io.terminus.parana.auth.util.ParanaFileLoader;
import io.terminus.parana.auth.util.Protocol;
import java.net.URL;
import javax.servlet.ServletContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class ParanaServletFileLoader implements ParanaFileLoader {
   private static final Logger log = LoggerFactory.getLogger(ParanaServletFileLoader.class);
   @Autowired(
      required = false
   )
   private ServletContext servletContext;

   public ParanaFileLoader.Resp load(String path) {
      if(this.servletContext == null) {
         throw new IllegalStateException("no servlet context found");
      } else {
         path = Protocol.removeProtocol(path, Protocol.SERVLET);
         ParanaFileLoader.Resp resp = new ParanaFileLoader.Resp();

         try {
            URL url = this.servletContext.getResource(path);
            resp.setContext(Resources.toByteArray(url));
            resp.setSign("UNSUPPORTED");
            return resp;
         } catch (Exception var4) {
            log.error("error when load servlet file: {}", path, var4);
            return ParanaFileLoader.Resp.NOT_FOUND;
         }
      }
   }

   public ParanaFileLoader.Resp load(String path, String sign) {
      return ParanaFileLoader.Resp.NOT_MODIFIED;
   }
}
