package io.terminus.parana.auth.util;

import com.github.kevinsawicki.http.HttpRequest;
import io.terminus.parana.auth.util.ParanaFileLoader;
import org.springframework.stereotype.Component;

@Component
public class ParanaHttpFileLoader implements ParanaFileLoader {
   public ParanaFileLoader.Resp load(String path) {
      HttpRequest request = HttpRequest.get(path);
      if(request.notFound()) {
         return ParanaFileLoader.Resp.NOT_FOUND;
      } else {
         ParanaFileLoader.Resp resp = new ParanaFileLoader.Resp();
         resp.setContext(request.bytes());
         resp.setSign(String.valueOf(request.lastModified()));
         return resp;
      }
   }

   public ParanaFileLoader.Resp load(String path, String sign) {
      HttpRequest request = HttpRequest.get(path);
      request.header("If-Modified-Since", sign);
      if(request.notFound()) {
         return ParanaFileLoader.Resp.NOT_FOUND;
      } else if(request.notModified()) {
         return ParanaFileLoader.Resp.NOT_MODIFIED;
      } else {
         ParanaFileLoader.Resp resp = new ParanaFileLoader.Resp();
         resp.setContext(request.bytes());
         resp.setSign(String.valueOf(request.lastModified()));
         return resp;
      }
   }
}
