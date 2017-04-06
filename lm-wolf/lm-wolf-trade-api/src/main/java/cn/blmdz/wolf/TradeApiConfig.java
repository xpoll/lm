package cn.blmdz.wolf;

import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;

@Configuration
@ComponentScan({"io.terminus.parana.order"})
public class TradeApiConfig {
   @Configuration
   @ComponentScan({"io.terminus.parana.cart"})
   public static class ShoppingCartConfiguration {
   }
}
