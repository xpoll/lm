package io.terminus.parana.auth.util;

import com.google.common.io.Files;
import io.terminus.parana.auth.util.ParanaFileLoader;
import io.terminus.parana.auth.util.Protocol;
import java.io.File;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

@Component
public class ParanaLocalFileLoader implements ParanaFileLoader {
   private static final Logger log = LoggerFactory.getLogger(ParanaLocalFileLoader.class);

   public ParanaFileLoader.Resp load(String path) {
      return this.load(path, (String)null);
   }

   public ParanaFileLoader.Resp load(String path, String sign) {
      if(path.startsWith(Protocol.FILE.getPrefix())) {
         path = Protocol.removeProtocol(path, Protocol.FILE);
      }

      File file = new File(path);
      if(!file.exists()) {
         return ParanaFileLoader.Resp.NOT_FOUND;
      } else if(sign != null && file.lastModified() == Long.valueOf(sign).longValue()) {
         return ParanaFileLoader.Resp.NOT_MODIFIED;
      } else {
         ParanaFileLoader.Resp resp = new ParanaFileLoader.Resp();
         resp.setSign(String.valueOf(file.lastModified()));

         try {
            resp.setContext(Files.toByteArray(file));
            return resp;
         } catch (Exception var6) {
            log.error("error when load local file: {}", path, var6);
            return ParanaFileLoader.Resp.NOT_FOUND;
         }
      }
   }
}
