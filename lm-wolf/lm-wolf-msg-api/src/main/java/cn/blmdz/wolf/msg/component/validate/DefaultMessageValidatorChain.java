package cn.blmdz.wolf.msg.component.validate;

import java.util.ArrayList;
import java.util.List;

import cn.blmdz.wolf.msg.component.MessageValidator;
import cn.blmdz.wolf.msg.component.MessageValidatorChain;
import cn.blmdz.wolf.msg.model.Message;

public class DefaultMessageValidatorChain implements MessageValidatorChain {
   public final List<MessageValidator> validators;

   public DefaultMessageValidatorChain() {
      this.validators = new ArrayList<MessageValidator>();
      this.validators.add(new PreMesssageValidator());
      this.validators.add(new EmailValidator());
      this.validators.add(new AppPushValidator());
      this.validators.add(new NotificationValidator());
      this.validators.add(new SmsValidator());
   }

   public DefaultMessageValidatorChain(List<MessageValidator> validators) {
      this.validators = validators;
   }

   public void check(Message message) {
      for(MessageValidator validator : this.validators) {
         validator.check(message);
      }

   }
}
