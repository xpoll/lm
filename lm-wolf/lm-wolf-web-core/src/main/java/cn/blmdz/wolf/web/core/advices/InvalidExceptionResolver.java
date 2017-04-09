package cn.blmdz.wolf.web.core.advices;

import com.google.common.net.MediaType;

import cn.blmdz.wolf.rule.exception.InvalidException;

import java.io.PrintWriter;
import javax.servlet.http.HttpServletResponse;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;

@ControllerAdvice
public class InvalidExceptionResolver {
   @ExceptionHandler({InvalidException.class})
   public void ruleChainErrorHandler(InvalidException e, HttpServletResponse response) throws Exception {
      response.setContentType(MediaType.JSON_UTF_8.toString());
      response.setStatus(e.getStatus());
      PrintWriter out = response.getWriter();
      Throwable var4 = null;

      try {
         out.print("属性校验失败");
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
