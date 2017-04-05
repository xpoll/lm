package io.terminus.parana.web.core.advices;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.net.MediaType;
import io.terminus.common.exception.JsonResponseException;
import io.terminus.common.utils.JsonMapper;
import java.io.PrintWriter;
import javax.servlet.http.HttpServletResponse;
import org.springframework.web.bind.annotation.ExceptionHandler;

public class JsonExceptionResolver {
   private ObjectMapper objectMapper = JsonMapper.nonEmptyMapper().getMapper();

   @ExceptionHandler({JsonResponseException.class})
   public void OPErrorHandler(JsonResponseException e, HttpServletResponse response) throws Exception {
      response.setContentType(MediaType.JSON_UTF_8.toString());
      response.setStatus(e.getStatus());
      PrintWriter out = response.getWriter();
      Throwable var4 = null;

      try {
         out.print(this.objectMapper.writeValueAsString(e.getMessage()));
      } catch (Throwable var13) {
         var4 = var13;
         throw var13;
      } finally {
         if(out != null) {
            if(var4 != null) {
               try {
                  out.close();
               } catch (Throwable var12) {
                  var4.addSuppressed(var12);
               }
            } else {
               out.close();
            }
         }

      }

   }
}
