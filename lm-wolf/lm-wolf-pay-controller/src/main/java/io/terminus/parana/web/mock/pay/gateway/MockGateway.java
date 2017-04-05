package io.terminus.parana.web.mock.pay.gateway;

import com.google.common.base.Preconditions;
import com.google.common.collect.Maps;
import io.terminus.parana.web.mock.pay.annotations.MockPay;
import io.terminus.parana.web.mock.pay.manager.AbstractMockGatewayManager;
import java.util.Map;
import java.util.regex.Pattern;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Component;

@Component
public class MockGateway {
   private static final Logger log = LoggerFactory.getLogger(MockGateway.class);
   @Autowired
   private ApplicationContext applicationContext;
   private Map channelMapping;

   public String serve(HttpServletRequest request, HttpServletResponse response, String channel) {
      this.initPayManagers();
      AbstractMockGatewayManager gatewayManager = this.getGatewayManager(channel);
      return gatewayManager.serve(request, response);
   }

   public Map payConfirm(HttpServletRequest request, String channel) {
      this.initPayManagers();
      AbstractMockGatewayManager gatewayManager = this.getGatewayManager(channel);
      return gatewayManager.payConfirm(request);
   }

   private AbstractMockGatewayManager getGatewayManager(String channel) {
      String handlerKey = channel;

      AbstractMockGatewayManager mockPayManager;
      try {
         this.checkPayManager(handlerKey);
         mockPayManager = (AbstractMockGatewayManager)this.channelMapping.get(handlerKey);
      } catch (RuntimeException var5) {
         throw new IllegalArgumentException("mock gateway channel[" + channel + "] not any pay manager found for illegal [payGroup] value", var5);
      }

      Preconditions.checkNotNull(mockPayManager, "Not any mock gateway manager found of  channel[%s], please checkMockGatewayManagers @PayChannels", new Object[]{channel});
      return mockPayManager;
   }

   private void checkPayManager(String handlerKey) {
      if(!this.channelMapping.containsKey(handlerKey)) {
         this.channelMapping.put(handlerKey, (Object)null);

         for(String key : this.channelMapping.keySet()) {
            if(Pattern.matches(key, handlerKey)) {
               this.channelMapping.put(handlerKey, this.channelMapping.get(key));
            }
         }
      }

   }

   private void initPayManagers() {
      if(this.channelMapping == null) {
         this.channelMapping = Maps.newHashMap();
         Map<String, Object> mockPayManagers = this.applicationContext.getBeansWithAnnotation(MockPay.class);

         for(String beanName : mockPayManagers.keySet()) {
            Object bean = mockPayManagers.get(beanName);
            Preconditions.checkState(bean instanceof AbstractMockGatewayManager, "Illegal Bean[%s] definition for not inherited from AbstractPayManager", new Object[]{bean});
            MockPay annotation = (MockPay)bean.getClass().getAnnotation(MockPay.class);
            String[] channels = annotation.channels();

            for(String channel : channels) {
               Preconditions.checkState(!this.channelMapping.containsKey(channel), "Illegal @MockPay definition of[%s] for duplicate channel[%s]", new Object[]{bean, channel});
               this.channelMapping.put(channel, bean);
            }
         }

      }
   }
}
