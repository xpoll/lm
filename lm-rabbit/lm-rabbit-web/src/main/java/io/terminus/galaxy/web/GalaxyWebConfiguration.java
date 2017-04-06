/*
 * Copyright (c) 2016. 杭州端点网络科技有限公司.  All rights reserved.
 */

package io.terminus.galaxy.web;

import com.google.common.eventbus.EventBus;
import io.terminus.galaxy.cacher.GalaxyShopCacher;
import io.terminus.galaxy.item.service.GalaxyItemReadService;
import io.terminus.galaxy.web.core.GalaxyCoreWebConfiguration;
import io.terminus.parana.ItemApiConfiguration;
import io.terminus.parana.TradeApiConfig;
import io.terminus.parana.auth.web.WebAuthenticationConfiguration;
import io.terminus.parana.search.item.ItemSearchWriteService;
import io.terminus.parana.shop.service.ShopReadService;
import io.terminus.parana.web.core.advices.JsonExceptionResolver;
import io.terminus.parana.web.core.events.item.listener.IndexListener;
import io.terminus.parana.web.msg.config.MsgWebConfig;
import io.terminus.parana.web.pay.config.WebPayConfig;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.FilterType;
import org.springframework.context.annotation.Import;
import org.springframework.web.servlet.config.annotation.EnableWebMvc;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurerAdapter;

/**
 * Author:  <a href="mailto:i@terminus.io">jlchen</a>
 * Date: 2016-02-01
 */
@Configuration
@ComponentScan(basePackages = {
        "io.terminus.parana.web.core.advices",
        "io.terminus.parana.web.core.component",
        "io.terminus.parana.web.core.events",
        "io.terminus.parana.web.core.exceptions",
        "io.terminus.parana.web.front.brand",
        "io.terminus.parana.web.front.category",
        "io.terminus.parana.web.front.component",
        "io.terminus.parana.web.front.design",
        "io.terminus.parana.web.front.item",
        "io.terminus.parana.web.front.spu",
        "io.terminus.parana.web.front.trade",
}, excludeFilters = {
        @ComponentScan.Filter(type = FilterType.ASSIGNABLE_TYPE, classes = {
                JsonExceptionResolver.class
        })
})
@EnableWebMvc
@Import({ItemApiConfiguration.class, TradeApiConfig.class, GalaxyCoreWebConfiguration.class,
        WebAuthenticationConfiguration.class,
        WebPayConfig.class,
        MsgWebConfig.class
})
public class GalaxyWebConfiguration extends WebMvcConfigurerAdapter {

    @ConditionalOnBean(ItemSearchWriteService.class)
    @Bean
    public IndexListener indexListener(ItemSearchWriteService itemSearchWriteService,
                                       EventBus eventBus) {
        return new IndexListener(itemSearchWriteService, eventBus);
    }

    @Bean
    public GalaxyShopCacher galaxyShopCacher(ShopReadService shopReadService,
                                             GalaxyItemReadService galaxyItemReadService,
                                             @Value("${cache.shop.duration-in-minutes:3}") Long duration) {
        return new GalaxyShopCacher(shopReadService, galaxyItemReadService, duration);
    }
}
