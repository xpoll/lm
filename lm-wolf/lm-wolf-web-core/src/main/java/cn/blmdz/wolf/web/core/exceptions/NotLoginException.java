package cn.blmdz.wolf.web.core.exceptions;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ResponseStatus;

@ResponseStatus(HttpStatus.UNAUTHORIZED)
public class NotLoginException extends RuntimeException {
   private static final long serialVersionUID = 5424651419263810916L;
   private String target;

   public NotLoginException(String target) {
      this.target = target;
   }

   public String getTarget() {
      return this.target;
   }
}
