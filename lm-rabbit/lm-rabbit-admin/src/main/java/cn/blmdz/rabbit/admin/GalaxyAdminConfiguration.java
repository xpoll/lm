/*
 * Copyright (c) 2016. 杭州端点网络科技有限公司.  All rights reserved.
 */

package cn.blmdz.rabbit.admin;

import org.springframework.beans.factory.annotation.Autowire;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.FilterType;
import org.springframework.context.annotation.Import;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.web.servlet.config.annotation.EnableWebMvc;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurerAdapter;

import cn.blmdz.rabbit.web.core.GalaxyCoreWebConfiguration;
import cn.blmdz.wolf.ItemApiConfiguration;
import cn.blmdz.wolf.auth.web.WebAuthenticationConfiguration;
import cn.blmdz.wolf.config.ConfigCenter;
import cn.blmdz.wolf.web.core.advices.JsonExceptionResolver;
import cn.blmdz.wolf.web.msg.config.MsgAdminWebConfig;
import cn.blmdz.wolf.web.msg.config.MsgWebConfig;

/**
 * Author  : panxin
 * Date    : 6:17 PM 2/29/16
 * Mail    : panxin@terminus.io
 */
@Configuration
@ComponentScan(basePackages = {
        "cn.blmdz.wolf.web.core.advices",
        "cn.blmdz.wolf.web.core.component",
        "cn.blmdz.wolf.web.core.events",
        "cn.blmdz.wolf.web.core.exceptions",
        "cn.blmdz.wolf.web.admin.brand",
        "cn.blmdz.wolf.web.admin.category",
        "cn.blmdz.wolf.web.admin.item",
        "cn.blmdz.wolf.web.admin.jobs",
        "cn.blmdz.wolf.web.admin.spu",
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
