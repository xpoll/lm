package io.terminus.parana.msg.component.validate;

import io.terminus.parana.msg.component.MessageValidator;
import io.terminus.parana.msg.component.MessageValidatorChain;
import io.terminus.parana.msg.component.validate.AppPushValidator;
import io.terminus.parana.msg.component.validate.EmailValidator;
import io.terminus.parana.msg.component.validate.NotificationValidator;
import io.terminus.parana.msg.component.validate.PreMesssageValidator;
import io.terminus.parana.msg.component.validate.SmsValidator;
import io.terminus.parana.msg.model.Message;
import java.util.ArrayList;
import java.util.List;

public class DefaultMessageValidatorChain implements MessageValidatorChain {
   public final List validators;

   public DefaultMessageValidatorChain() {
      this.validators = new ArrayList();
      this.validators.add(new PreMesssageValidator());
      this.validators.add(new EmailValidator());
      this.validators.add(new AppPushValidator());
      this.validators.add(new NotificationValidator());
      this.validators.add(new SmsValidator());
   }

   public DefaultMessageValidatorChain(List validators) {
      this.validators = validators;
   }

   public void check(Message message) {
      for(MessageValidator validator : this.validators) {
         validator.check(message);
      }

   }
}
