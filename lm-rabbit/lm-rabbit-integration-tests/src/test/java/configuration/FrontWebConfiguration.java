package configuration;

import io.terminus.galaxy.interceptor.MockLoginInterceptor;
import io.terminus.galaxy.order.GalaxyTradeConfiguration;
import io.terminus.parana.ItemApiConfiguration;
import io.terminus.parana.ItemAutoConfig;
import io.terminus.parana.TradeAutoConfig;
import io.terminus.parana.user.UserAutoConfig;
import io.terminus.parana.web.front.FrontConfiguration;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
import org.springframework.web.servlet.config.annotation.InterceptorRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurerAdapter;

/**
 * Mail: xiao@terminus.io <br>
 * Date: 2016-03-30 11:12 AM  <br>
 * Author: xiao
 */
@Configuration
@Import({
        ItemAutoConfig.class,
        GalaxyTradeConfiguration.class,
        UserAutoConfig.class,
        FrontConfiguration.class,
        ItemApiConfiguration.class
})

@ComponentScan({
        "io.terminus.galaxy.web.core.trade"
})
public class FrontWebConfiguration extends WebMvcConfigurerAdapter {

    @Override
    public void addInterceptors(InterceptorRegistry registry) {
        registry.addInterceptor(new MockLoginInterceptor());
    }
}
