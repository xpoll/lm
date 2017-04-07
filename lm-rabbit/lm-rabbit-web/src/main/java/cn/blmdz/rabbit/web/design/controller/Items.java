/*
 * <!--
 *   ~ Copyright (c) 2014 杭州端点网络科技有限公司
 *   -->
 */

package cn.blmdz.rabbit.web.design.controller;


import java.util.concurrent.TimeUnit;

import javax.annotation.PostConstruct;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import com.google.common.base.Objects;
import com.google.common.base.Throwables;
import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;

import cn.blmdz.home.common.exception.JsonResponseException;
import cn.blmdz.home.common.exception.ServiceException;
import cn.blmdz.home.common.model.BaseUser;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.hunt.common.UserUtil;
import cn.blmdz.hunt.design.service.ItemCustomService;
import cn.blmdz.rabbit.web.util.SafeHtmlValidator;
import cn.blmdz.wolf.parana.item.model.Item;
import cn.blmdz.wolf.parana.item.service.ItemReadService;
import cn.blmdz.wolf.parana.item.service.ItemWriteService;
import cn.blmdz.wolf.parana.shop.model.Shop;
import cn.blmdz.wolf.parana.shop.service.ShopReadService;

/**
 * Created by IntelliJ IDEA.
 * User: AnsonChan
 * Date: 13-12-17
 */
@Controller("designItems")
@RequestMapping("/api/design")
public class Items {
    private final static Logger log = LoggerFactory.getLogger(Items.class);

    @Autowired(required = false)
    private ItemCustomService itemCustomService;

    @Autowired(required = false)
    private ItemReadService itemReadService;

    @Autowired(required = false)
    private ItemWriteService itemWriteService;

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

    /**
     *
     * @param itemId 商品id
     * @param template html内容
     */
    @RequestMapping(value = "/items/{itemId}", method = RequestMethod.PUT, produces = MediaType.APPLICATION_JSON_VALUE)
    @ResponseBody
    public void saveItemCustom(@PathVariable Long itemId, @RequestParam String template) {
        try {
            if (template.length() > 50 * 1024) {
                log.error("template length is {},and long than 10k", template.length());
                throw new JsonResponseException("content.too.long");
            }
            if (SafeHtmlValidator.checkScriptAndEvent(template)) {
                log.error("has invalid html content: template({})", template);
                throw new JsonResponseException(403, "invalid.html");
            }

            BaseUser user = UserUtil.getCurrentUser();

            Response<Item> itemR = itemReadService.findById(itemId);
            Shop shop=shopCache.getUnchecked(itemR.getResult().getShopId());
            if (!itemR.isSuccess()) {
                log.error("failed to find item(id={}) error code:{}", itemId, itemR.getError());
                throw new JsonResponseException(itemR.getError());
            }
            if (!Objects.equal(shop.getUserId(), user.getId())) {
                throw new JsonResponseException(403, "item not belong to u");
            }
            itemCustomService.save(itemId, template);
        }catch (Exception e) {
            log.error("fail to save item custom item id={}, cause:{}", itemId, Throwables.getStackTraceAsString(e));
            throw new JsonResponseException("save.item.custom.fail");
        }
    }

    @RequestMapping(value = "/item-templates/{spuId}", method = RequestMethod.PUT, produces = MediaType.APPLICATION_JSON_VALUE)
    @ResponseBody
    public void saveItemTemplate(@PathVariable Long spuId, @RequestParam String template) {
//        User.TYPE userType = User.TYPE.fromNumber(UserUtil.getCurrentUser().getType());
//        if (userType != User.TYPE.ADMIN && userType != User.TYPE.SITE_OWNER) {
//            throw new JsonResponseException(403, "no required auth");
//        }
        if (SafeHtmlValidator.checkScriptAndEvent(template)){
            log.error("has invalid html content: template({})", template);
            throw new JsonResponseException(403, "invalid.html");
        }
        try {
            itemCustomService.saveTemplate(spuId, template);
        }catch (Exception e) {
            log.error("fail to save item template by spuId={}, template={}, cause:{}",
                    spuId, template, Throwables.getStackTraceAsString(e));
            throw new JsonResponseException("item.template.save.fail");
        }
    }
}
