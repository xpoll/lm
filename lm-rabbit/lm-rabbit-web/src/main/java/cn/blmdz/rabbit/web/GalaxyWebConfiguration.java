package cn.blmdz.rabbit.web;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.ComponentScan.Filter;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.FilterType;
import org.springframework.context.annotation.Import;
import org.springframework.web.servlet.config.annotation.EnableWebMvc;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurerAdapter;

import com.google.common.eventbus.EventBus;

import cn.blmdz.rabbit.cacher.GalaxyShopCacher;
import cn.blmdz.rabbit.item.service.GalaxyItemReadService;
import cn.blmdz.rabbit.web.core.GalaxyCoreWebConfiguration;
import cn.blmdz.wolf.ItemApiConfiguration;
import cn.blmdz.wolf.TradeApiConfig;
import cn.blmdz.wolf.auth.web.WebAuthenticationConfiguration;
import cn.blmdz.wolf.search.item.ItemSearchWriteService;
import cn.blmdz.wolf.shop.service.ShopReadService;
import cn.blmdz.wolf.web.core.advices.JsonExceptionResolver;
import cn.blmdz.wolf.web.core.events.item.listener.IndexListener;
import cn.blmdz.wolf.web.msg.config.MsgWebConfig;
import cn.blmdz.wolf.web.pay.config.WebPayConfig;

@Configuration
@EnableWebMvc
@ComponentScan(
		basePackages = {
		"cn.blmdz.wolf.web.core.advices",
		"cn.blmdz.wolf.web.core.component",
		"cn.blmdz.wolf.web.core.events",
		"cn.blmdz.wolf.web.core.exceptions",
		"cn.blmdz.wolf.web.front.brand",
		"cn.blmdz.wolf.web.front.category",
		"cn.blmdz.wolf.web.front.component",
		"cn.blmdz.wolf.web.front.design",
		"cn.blmdz.wolf.web.front.item",
		"cn.blmdz.wolf.web.front.spu",
		"cn.blmdz.wolf.web.front.trade",
		"cn.blmdz.hunt.engine"
		},
		excludeFilters = {
		@Filter(
				type = FilterType.ASSIGNABLE_TYPE,
				classes = JsonExceptionResolver.class)
		})
@Import({
	ItemApiConfiguration.class,
	TradeApiConfig.class,
	GalaxyCoreWebConfiguration.class,
	WebAuthenticationConfiguration.class,
	WebPayConfig.class,
	MsgWebConfig.class
	})
public class GalaxyWebConfiguration extends WebMvcConfigurerAdapter {

	@Bean
	@ConditionalOnBean(ItemSearchWriteService.class)
	public IndexListener indexListener(ItemSearchWriteService itemSearchWriteService, EventBus eventBus) {
		return new IndexListener(itemSearchWriteService, eventBus);
	}

	@Bean
	public GalaxyShopCacher galaxyShopCacher(ShopReadService shopReadService,
			GalaxyItemReadService galaxyItemReadService, @Value("${cache.shop.duration-in-minutes:3}") Long duration) {
		return new GalaxyShopCacher(shopReadService, galaxyItemReadService, duration);
	}
}
