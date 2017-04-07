/*
 * Copyright (c) 2016. 杭州端点网络科技有限公司.  All rights reserved.
 */

package cn.blmdz.rabbit.web.front.interceptors;

import java.util.List;
import java.util.concurrent.TimeUnit;

import javax.annotation.PostConstruct;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.web.servlet.handler.HandlerInterceptorAdapter;

import com.google.common.base.Throwables;
import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.home.common.util.Splitters;
import cn.blmdz.home.zookeeper.pubsub.SubscribeCallback;
import cn.blmdz.home.zookeeper.pubsub.Subscriber;
import cn.blmdz.hunt.common.UserUtil;
import cn.blmdz.hunt.engine.common.WebUtil;
import cn.blmdz.rabbit.common.enums.UserRole;
import cn.blmdz.rabbit.common.enums.UserType;
import cn.blmdz.rabbit.common.enums.ZkMsg;
import cn.blmdz.rabbit.user.model.SubSeller;
import cn.blmdz.rabbit.user.service.SellerReadService;
import cn.blmdz.rabbit.user.util.GalaxyUserMaker;
import cn.blmdz.wolf.common.model.ParanaUser;
import cn.blmdz.wolf.common.utils.Strs;
import cn.blmdz.wolf.parana.shop.model.Shop;
import cn.blmdz.wolf.parana.shop.service.ShopReadService;
import cn.blmdz.wolf.user.model.User;
import cn.blmdz.wolf.user.service.UserReadService;
import cn.blmdz.wolf.web.core.Constants;
import lombok.val;
import lombok.extern.slf4j.Slf4j;

/**
 * Author:  <a href="mailto:i@terminus.io">jlchen</a>
 * Date: 2016-01-28
 */
@Slf4j
@Component("loginInterceptor")
public class GalaxyLoginInterceptor extends HandlerInterceptorAdapter {

    private final LoadingCache<Long, Response<User>> userCache;

    private final UserReadService<User> userReadService;

    private final ShopReadService shopReadService;

    private final SellerReadService sellerReadService;

    @Autowired(required = false)
    protected Subscriber zkListener;

    @Autowired
    public GalaxyLoginInterceptor(final UserReadService<User> userReadService, ShopReadService shopReadService, SellerReadService sellerReadService) {
        this.userReadService = userReadService;
        userCache = CacheBuilder.newBuilder().expireAfterWrite(30, TimeUnit.MINUTES).build(new CacheLoader<Long, Response<User>>() {
            @Override
            public Response<User> load(Long userId) throws Exception {
                return userReadService.findById(userId);
            }
        });
        this.shopReadService = shopReadService;
        this.sellerReadService = sellerReadService;
    }

    @PostConstruct
    public void init() {
        if (zkListener != null) {
            try {
                zkListener.subscribe(new SubscribeCallback() {
                    @Override
                    public void fire(byte[] bytes) {
                        String key = new String(bytes);
                        List<String> parts = Splitters.COLON.splitToList(key);
                        if (parts.size() == 2 && ZkMsg.CLEAR_USER_CACHE.equals(parts.get(0))) {
                            Long userId = Strs.parseLong(parts.get(1)).orNull();
                            if (userId == null) {
                                return;
                            }
                            userCache.invalidate(userId);
                        }
                    }
                });
            } catch (Exception e) {
                log.warn("subscribe user changed notice failed, cause:{}", Throwables.getStackTraceAsString(e));
            }
        }
    }

    @Override
    public boolean preHandle(HttpServletRequest request, HttpServletResponse response, Object handler) throws Exception {
        WebUtil.putRequestAndResponse(request, response);

        HttpSession session = request.getSession(false);
        if (session != null) {
            Object userIdInSession = session.getAttribute(Constants.SESSION_USER_ID);
            if (userIdInSession != null) {

                final Long userId = Long.valueOf(userIdInSession.toString());
                Response<? extends User> result = userReadService.findById(userId);
                if (!result.isSuccess()) {
                    log.warn("failed to find user where id={},error code:{}", userId, result.getError());
                    return false;
                }
                User user = result.getResult();
                if (user != null) {
                    ParanaUser paranaUser = GalaxyUserMaker.from(user);
                    thinkShopId(paranaUser);
                    UserUtil.putCurrentUser(paranaUser);
                }
            }
        }
        return true;
    }

    @Override
    public void afterCompletion(HttpServletRequest request, HttpServletResponse response, Object handler, Exception ex) throws Exception {
        WebUtil.clear();
        UserUtil.clearCurrentUser();
    }

    private void thinkShopId(ParanaUser mutableUser) {
        thinkMainSellerShopId(mutableUser);
        thinkSubSellerShopId(mutableUser);
    }

    private void thinkMainSellerShopId(ParanaUser u) {
        if (u.getType() == null || u.getType() != UserType.NORMAL.value()
                || !u.getRoles().contains(UserRole.SELLER.name())) {
            return;
        }
        Shop shop = findShop(u.getId());
        if (shop != null) {
            u.setPresentUserId(shop.getUserId());
            u.setShopId(shop.getId());
        }
    }

    private Shop findShop(Long userId) {
        Response<Shop> rShop = shopReadService.findByUserId(userId);
        if (!rShop.isSuccess()) {
            log.error("failed to find shop for user(id={}), error code:{}", userId, rShop.getError());
            return null;
        }
        return rShop.getResult();
    }

    private void thinkSubSellerShopId(ParanaUser u) {
        if (u.getType() == null || u.getType() != UserType.SUB_ACCOUNT.value()
                || !u.getRoles().contains(UserRole.SELLER.name())) {
            return;
        }
        val rSub = sellerReadService.findSubSellerByUserId(u.getId());
        if (!rSub.isSuccess()) {
            log.warn("find sub seller failed, userId={}, error={}", u.getId(), rSub.getError());
            return;
        }
        SubSeller sub = rSub.getResult().orNull();
        Long shopId = sub == null ? null : sub.getShopId();
        if (shopId != null) {
            val rShop = shopReadService.findById(shopId);
            if (!rShop.isSuccess()) {
                log.error("failed to find shop by id={}, error={}", shopId, rShop.getError());
                return;
            }
            Shop shop = rShop.getResult();
            u.setShopId(shopId);
            u.setPresentUserId(shop.getUserId());
        }
    }
}
