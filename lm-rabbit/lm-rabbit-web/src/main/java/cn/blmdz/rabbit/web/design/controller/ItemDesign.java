/*
* <!--
*   ~ Copyright (c) 2014 杭州端点网络科技有限公司
*   -->
*/

package cn.blmdz.rabbit.web.design.controller;


import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;

import javax.annotation.PostConstruct;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import com.google.api.client.util.Objects;
import com.google.common.base.Throwables;
import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import com.google.common.collect.Maps;

import cn.blmdz.home.common.exception.JsonResponseException;
import cn.blmdz.home.common.exception.ServiceException;
import cn.blmdz.home.common.model.BaseUser;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.hunt.common.UserUtil;
import cn.blmdz.hunt.engine.config.model.Render;
import cn.blmdz.hunt.engine.service.ConfigService;
import cn.blmdz.rabbit.web.design.modal.EcpAppKey;
import cn.blmdz.rabbit.web.design.modal.EcpLayoutType;
import cn.blmdz.wolf.parana.item.model.Item;
import cn.blmdz.wolf.parana.item.service.ItemReadService;
import cn.blmdz.wolf.parana.shop.model.Shop;
import cn.blmdz.wolf.parana.shop.service.ShopReadService;
import lombok.extern.slf4j.Slf4j;

/**
 * Created by IntelliJ IDEA.
 * User: AnsonChan
 * Date: 13-12-16
 */
@Controller
@RequestMapping("/design")
@Slf4j
public class ItemDesign {
    @Autowired(required = false)
    private ItemReadService itemReadService;
    @Autowired
    private ConfigService configService;
    @Autowired(required = false)
    private ShopReadService shopReadService;

    private LoadingCache<Long, Shop> shopCache;

    @PostConstruct
    public void init() {
        shopCache = CacheBuilder.newBuilder()
                .expireAfterWrite(30, TimeUnit.MINUTES)
                .build(new CacheLoader<Long, Shop>() {
                    @Override
                    public Shop load(Long shopId) throws Exception {
                        Response<Shop> findShop = shopReadService.findById(shopId);
                        if (!findShop.isSuccess()) {
                            throw new ServiceException(findShop.getError());
                        }
                        return findShop.getResult();
                    }
                });
    }

    @RequestMapping(value = "/items/{itemId}", method = RequestMethod.GET)
    public String designItem(@PathVariable Long itemId, Map<String, Object> context) {
        try {
            BaseUser user = UserUtil.getCurrentUser();
            Response<Item> itemR = itemReadService.findById(itemId);
            if (!itemR.isSuccess()) {
                log.error("fail to find item by id={}, error code:{}", itemId, itemR.getError());
                throw new JsonResponseException(itemR.getError());
            }
            Item item = itemR.getResult();
            Shop shop=shopCache.getUnchecked(item.getShopId());
            if (!Objects.equal(user.getId(), shop.getUserId())) {
                throw new JsonResponseException(403, "item not belong to u");
            }

            Map<String, Object> editorContext = Maps.newHashMap();
            editorContext.put("mode", "ITEM");
            editorContext.put("noPage", true);
            editorContext.put("itemId", itemId);
            editorContext.put("spuId", item.getSpuId());
            editorContext.put("detailType", 0);
            editorContext.put("naiveUrl", "/items/" + itemId);

            context.put("editorContext", editorContext);
            context.put("app", EcpAppKey.PC.name());
            context.put("title", "商品详情编辑");
            return configService.getDefaultFrontConfig().getRender().getEditorLayout();
        }catch (Exception e) {
            log.error("fail to design item id={}, cause:{}", itemId, Throwables.getStackTraceAsString(e));
            throw new JsonResponseException("design.item.fail");
        }
    }

    @RequestMapping(value = "/item-templates/{spuId}", method = RequestMethod.GET)
    public String designTemplate(@PathVariable Long spuId, Map<String, Object> context) {
//        Map<String, Render.Layout> layouts = ecpSiteService.list(EcpRenderMode.PC_SHOP);
        List<Render.Layout> layouts = configService.listLayouts(EcpAppKey.PC.name(), EcpLayoutType.SHOP.name());
        Render.Layout layout = layouts.isEmpty() ? null : layouts.get(0);
        if (layout == null) {
            throw new IllegalStateException("no default layout found for PC");
        }

        Map<String, Object> editorContext = Maps.newHashMap();
        editorContext.put("mode", "ITEM_TEMPLATE");
        editorContext.put("noPage", true);
        editorContext.put("spuId", spuId);
        editorContext.put("naiveUrl", layout.getRoot() + "/" + "default-item" + "?spuId=" + spuId);

        context.put("editorContext", editorContext);
        context.put("app", EcpAppKey.PC.name());
        context.put("title", "模板商品详情编辑");
        return configService.getDefaultFrontConfig().getRender().getEditorLayout();
    }
}
