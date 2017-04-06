/*
 * Copyright (c) 2016. 杭州端点网络科技有限公司.  All rights reserved.
 */

package io.terminus.galaxy.web.admin;

import io.terminus.galaxy.web.core.GalaxyCoreWebConfiguration;
import io.terminus.parana.ItemApiConfiguration;
import io.terminus.parana.auth.web.WebAuthenticationConfiguration;
import io.terminus.parana.config.ConfigCenter;
import io.terminus.parana.web.core.advices.JsonExceptionResolver;
import io.terminus.parana.web.msg.config.MsgAdminWebConfig;
import io.terminus.parana.web.msg.config.MsgWebConfig;
import org.springframework.beans.factory.annotation.Autowire;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.FilterType;
import org.springframework.context.annotation.Import;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.web.servlet.config.annotation.EnableWebMvc;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurerAdapter;

/**
 * Author  : panxin
 * Date    : 6:17 PM 2/29/16
 * Mail    : panxin@terminus.io
 */
@Configuration
@ComponentScan(basePackages = {
        "io.terminus.parana.web.core.advices",
        "io.terminus.parana.web.core.component",
        "io.terminus.parana.web.core.events",
        "io.terminus.parana.web.core.exceptions",
        "io.terminus.parana.web.admin.brand",
        "io.terminus.parana.web.admin.category",
        "io.terminus.parana.web.admin.item",
        "io.terminus.parana.web.admin.jobs",
        "io.terminus.parana.web.admin.spu",
}, excludeFilters = {
        @ComponentScan.Filter(type = FilterType.ASSIGNABLE_TYPE, classes = {
                JsonExceptionResolver.class,
        })
})
@EnableWebMvc
@EnableScheduling
@Import({ItemApiConfiguration.class, GalaxyCoreWebConfiguration.class,
        WebAuthenticationConfiguration.class,
        MsgAdminWebConfig.class, MsgWebConfig.class})
public class GalaxyAdminConfiguration extends WebMvcConfigurerAdapter {

    @Bean(autowire = Autowire.BY_NAME)
    public ConfigCenter configCenter() {
        return new ConfigCenter();
    }
}
