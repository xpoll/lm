package cn.blmdz.wolf.web.core.advices;

import java.io.PrintWriter;

import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.base.Throwables;

import cn.blmdz.home.common.util.JsonMapper;

@ControllerAdvice
public class NPEResolver {
   private static final Logger log = LoggerFactory.getLogger(NPEResolver.class);
   private ObjectMapper objectMapper = JsonMapper.nonEmptyMapper().getMapper();

   @ExceptionHandler({NullPointerException.class})
   public void OPErrorHandler(NullPointerException e, HttpServletResponse response) throws Exception {
      response.setStatus(HttpStatus.INTERNAL_SERVER_ERROR.value());
      PrintWriter out = response.getWriter();
      Throwable var4 = null;

      try {
         log.error("oops, NPE {} caught", Throwables.getStackTraceAsString(e));
         out.write(this.objectMapper.writeValueAsString(e));
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
