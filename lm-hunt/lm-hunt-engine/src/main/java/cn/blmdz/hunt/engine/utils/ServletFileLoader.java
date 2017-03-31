package cn.blmdz.hunt.engine.utils;

import java.net.URL;

import javax.servlet.ServletContext;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.google.common.io.Resources;

@Component
public class ServletFileLoader implements FileLoader {
   private static final Logger log = LoggerFactory.getLogger(ServletFileLoader.class);
   @Autowired(
      required = false
   )
   private ServletContext servletContext;

   public FileLoader.Resp load(String path) {
      if(this.servletContext == null) {
         throw new IllegalStateException("no servlet context found");
      } else {
         path = Protocol.removeProtocol(path, Protocol.SERVLET);
         FileLoader.Resp resp = new FileLoader.Resp();

         try {
            URL url = this.servletContext.getResource(path);
            resp.setContext(Resources.toByteArray(url));
            resp.setSign("UNSUPPORTED");
            return resp;
         } catch (Exception var4) {
            log.error("error when load servlet file: {}", path, var4);
            return FileLoader.Resp.NOT_FOUND;
         }
      }
   }

   public FileLoader.Resp load(String path, String sign) {
      return FileLoader.Resp.NOT_MODIFIED;
   }
}
