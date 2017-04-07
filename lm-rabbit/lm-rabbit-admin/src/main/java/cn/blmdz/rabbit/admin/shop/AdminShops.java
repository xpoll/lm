package cn.blmdz.rabbit.admin.shop;

import java.util.Objects;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import com.google.common.eventbus.EventBus;

import cn.blmdz.home.common.exception.JsonResponseException;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.parana.shop.model.Shop;
import cn.blmdz.wolf.parana.shop.service.AdminShopWriteService;
import cn.blmdz.wolf.parana.shop.service.ShopReadService;
import cn.blmdz.wolf.web.core.events.shop.ShopFrozenEvent;
import cn.blmdz.wolf.web.core.events.shop.ShopUnfrozenEvent;
import lombok.extern.slf4j.Slf4j;

/**
 * Date: 6/30/16
 * Time: 10:46 AM
 * Author: 2016年 <a href="mailto:d@terminus.io">张成栋</a>
 */
@Slf4j
@RestController
@RequestMapping("/api/admin/shops")
public class AdminShops {
    private final EventBus eventBus;
    private final ShopReadService shopReadService;
    private final AdminShopWriteService adminShopWriteService;

    @Autowired
    public AdminShops(EventBus eventBus,
                      ShopReadService shopReadService,
                      AdminShopWriteService adminShopWriteService) {
        this.eventBus = eventBus;
        this.shopReadService = shopReadService;
        this.adminShopWriteService = adminShopWriteService;
    }

    /**
     * 根据id冻结店铺, 不存在就报错
     * @param id 店铺id
     */
    @RequestMapping(value = "/{id}/frozen", method = RequestMethod.PUT)
    public Boolean frozen(@PathVariable Long id) {
        Response<Shop> findShop = shopReadService.findById(id);
        if (!findShop.isSuccess()) {
            log.error("fail to find shop by id:{}, cause:{}", id, findShop.getError());
            throw new JsonResponseException("shop.not.exist");
        }

        if (Objects.equals(findShop.getResult().getStatus(), -1)) {
            log.warn("shop already frozen.");
            return Boolean.TRUE;
        }

        Response<Boolean> tryFrozen = adminShopWriteService.frozen(id);
        if (!tryFrozen.isSuccess()) {
            log.error("fail to frozen shop by id:{}, cause:{}", id, tryFrozen.getError());
            throw new JsonResponseException(tryFrozen.getError());
        }

        eventBus.post(new ShopFrozenEvent(id, null));
        return Boolean.TRUE;
    }

    /**
     * 根据id解冻店铺, 不存在就报错
     * @param id 店铺id
     */
    @RequestMapping(value = "/{id}/unfrozen", method = RequestMethod.PUT)
    public Boolean unfrozen(@PathVariable Long id) {
        Response<Shop> findShop = shopReadService.findById(id);
        if (!findShop.isSuccess()) {
            log.error("fail to find shop by id:{}, cause:{}", id, findShop.getError());
            throw new JsonResponseException("shop.not.exist");
        }

        if (!Objects.equals(findShop.getResult().getStatus(), -2)) {
            log.warn("shop already unfrozen.");
            return Boolean.TRUE;
        }

        Response<Boolean> tryUnrozen = adminShopWriteService.unFrozen(id);
        if (!tryUnrozen.isSuccess()) {
            log.error("fail to unfrozen shop by id:{}, cause:{}", id, tryUnrozen.getError());
            throw new JsonResponseException(tryUnrozen.getError());
        }

        eventBus.post(new ShopUnfrozenEvent(id, null));
        return Boolean.TRUE;
    }
}
