package configuration;

import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
import org.springframework.web.servlet.config.annotation.InterceptorRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurerAdapter;

import cn.blmdz.rabbit.interceptor.MockLoginInterceptor;
import cn.blmdz.rabbit.order.GalaxyTradeConfiguration;
import cn.blmdz.wolf.ItemAutoConfig;
import cn.blmdz.wolf.parana.ItemApiConfiguration;
import cn.blmdz.wolf.user.UserAutoConfig;
import cn.blmdz.wolf.web.front.FrontConfiguration;

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
