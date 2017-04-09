/*
 * <!--
 *   ~ Copyright (c) 2014 杭州端点网络科技有限公司
 *   -->
 */

package cn.blmdz.rabbit.web.design.controller;

import java.util.Map;
import java.util.concurrent.TimeUnit;

import javax.annotation.PostConstruct;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import com.google.common.base.CharMatcher;
import com.google.common.base.Supplier;
import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;

import cn.blmdz.home.common.exception.JsonResponseException;
import cn.blmdz.home.common.exception.ServiceException;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.hunt.common.UserUtil;
import cn.blmdz.hunt.design.container.DPageRender;
import cn.blmdz.hunt.design.medol.Site;
import cn.blmdz.hunt.design.service.SiteService;
import cn.blmdz.hunt.engine.RenderConstants;
import cn.blmdz.hunt.engine.ThreadVars;
import cn.blmdz.hunt.engine.exception.NotFound404Exception;
import cn.blmdz.hunt.engine.request.ViewRender;
import cn.blmdz.hunt.engine.utils.Domains;
import cn.blmdz.rabbit.web.design.modal.EcpAppKey;
import cn.blmdz.rabbit.web.design.modal.ShopSitePageCategory;
import cn.blmdz.rabbit.web.design.service.EcpSiteService;
import cn.blmdz.wolf.item.model.Item;
import cn.blmdz.wolf.item.service.ItemReadService;
import cn.blmdz.wolf.shop.model.Shop;
import cn.blmdz.wolf.shop.service.ShopReadService;


/**
 * Desc: 特殊 url 入口
 * Date: 8/16/12 10:48 AM
 * Created by yangzefeng on 14/11/19
 */
@Controller
public class GalaxyView {

    public static final Logger log = LoggerFactory.getLogger(GalaxyView.class);

    @Autowired(required = false)
    private DPageRender dPageRender;
    @Autowired
    private ViewRender viewRender;
    @Autowired
    private EcpSiteService ecpSiteService;
    @Autowired(required = false)
    private SiteService siteService;
    @Autowired(required = false)
    private ItemReadService itemReadService;
    @Autowired(required = false)
    private ShopReadService shopReadService;
//    @Autowired(required = false)
//    private SubDomainReadService subDomainReadService;

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
     * Item页面渲染入口
     *
     * @param itemId 商品ID
     */
    @RequestMapping(value = "/items/{itemId}", method = RequestMethod.GET)
    public void item(HttpServletRequest request, HttpServletResponse response,
                     @PathVariable Long itemId, @RequestParam Map<String, Object> context) {
        context.put("itemId", itemId);
        Response<Item> itemR = itemReadService.findById(itemId);
        if(!itemR.isSuccess()){
            log.error("failed to find item(id={}),error code:{}",itemId,itemR.getError());
            throw new NotFound404Exception(itemR.getError());
        }
        Item item = itemR.getResult();
        Shop shop=shopCache.getUnchecked(item.getShopId());
        context.put("spuId", item.getSpuId());
        context.put("buyerId", UserUtil.getUserId());
        context.put("shopId", item.getShopId());
        context.put("sellerId", shop.getUserId());
        renderShop(request, response, shop.getUserId(), ShopSitePageCategory.ITEM.getPath(), context);
    }




    @RequestMapping(value = "/shops/{shopKey}", method = RequestMethod.GET)
    public void shop(HttpServletRequest request, HttpServletResponse response,
                     @PathVariable String shopKey, @RequestParam Map<String, Object> params, Map<String, Object> context) {
        context.putAll(params);
        _shopPath(request, response, getShopIdByKey(shopKey), "index", context);
    }

    @RequestMapping(value = "/s/{shopKey}", method = RequestMethod.GET)
    public void shortShop(HttpServletRequest request, HttpServletResponse response,
                          @PathVariable String shopKey, @RequestParam Map<String, Object> params, Map<String, Object> context) {
        context.putAll(params);
        _shopPath(request, response, getShopIdByKey(shopKey), "index", context);
    }

    @RequestMapping(value = "/shops/{shopKey}/**", method = RequestMethod.GET)
    public void shopPath(HttpServletRequest request, HttpServletResponse response,
                         @PathVariable String shopKey, @RequestParam Map<String, Object> params, Map<String, Object> context) {
        String path = request.getRequestURI().substring(request.getContextPath().length() + ("/shops/" + shopKey + "/").length());
        context.putAll(params);
        _shopPath(request, response, getShopIdByKey(shopKey), path, context);
    }

    @RequestMapping(value = "/s/{shopKey}/**", method = RequestMethod.GET)
    public void shortShopPath(HttpServletRequest request, HttpServletResponse response,
                              @PathVariable String shopKey, @RequestParam Map<String, Object> params, Map<String, Object> context) {
        String path = request.getRequestURI().substring(request.getContextPath().length() + ("/shops/" + shopKey + "/").length());
        context.putAll(params);
        _shopPath(request, response, getShopIdByKey(shopKey), path, context);
    }

    private Long getShopIdByKey(String shopKey) {
        if (CharMatcher.JAVA_DIGIT.matchesAllOf(shopKey)) {
            return Long.valueOf(shopKey);
        } else {
//            Response<Optional<SubDomain>> subDomainR = subDomainReadService.getByDomainAllowNotFound(shopKey, SubDomainType.SHOP);
//            if (!subDomainR.isSuccess()) {
//                log.error("subDomain found failed with key={}, error code={}", shopKey, subDomainR.getError());
//                throw new JsonResponseException(subDomainR.getError());
//            }
//            if (!subDomainR.getResult().isPresent()) {
//                log.warn("subDomain not found with key={}, can not visit", shopKey);
//                throw new NotFound404Exception();
//            }
//            return subDomainR.getResult().get().getTargetId();
            return null;
        }
    }

    private void _shopPath(HttpServletRequest request, HttpServletResponse response,
                           Long shopId, String path, Map<String, Object> context) {
        Response<Shop> shopR = shopReadService.findById(shopId);
        if(!shopR.isSuccess()) {
            log.error("shop not found with id={}, error code={}", shopId, shopR.getError());
            throw new JsonResponseException(shopR.getError());
        }
        //if shop status frozen return 404
//        if(equalWith(shopR.getResult().getStatus(), Shop.Status.FROZEN.value())) {
//            log.warn("shop(id={}) is frozen, can not visit", shopR.getResult().getId());
//            throw new NotFound404Exception();
//        }
        context.put("shopId", shopId);
        renderShop(request, response, shopR.getResult().getUserId(), path, context);
    }

    private void renderShop(HttpServletRequest request, HttpServletResponse response, Long sellerId, final String path, final Map<String, Object> context) {
        final Long siteId = ecpSiteService.getCurrentShopSiteIdByApp(sellerId, EcpAppKey.valueOf(ThreadVars.getAppKey()));
        context.put("sellerId", sellerId);
        String domain = Domains.getDomainFromRequest(request);
        Site mainSite = siteService.findByDomain(domain);
        if (mainSite != null) {
            context.put(RenderConstants.MAIN_SITE, mainSite);
        }
        viewRender.render(request, response, new Supplier<String>() {
            @Override
            public String get() {
                return dPageRender.render(siteId, path, context);
            }
        });
    }
}
