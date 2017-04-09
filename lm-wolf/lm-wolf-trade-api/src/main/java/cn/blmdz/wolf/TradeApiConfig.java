package cn.blmdz.wolf;

import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;

@Configuration
@ComponentScan({"cn.blmdz.wolf.order"})
public class TradeApiConfig {
   @Configuration
   @ComponentScan({"cn.blmdz.wolf.cart"})
   public static class ShoppingCartConfiguration {
   }
}
