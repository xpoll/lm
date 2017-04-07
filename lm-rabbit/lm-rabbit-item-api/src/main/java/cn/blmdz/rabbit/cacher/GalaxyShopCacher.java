package cn.blmdz.rabbit.cacher;

import java.util.concurrent.TimeUnit;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;

import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;

import cn.blmdz.home.common.exception.ServiceException;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.hunt.protocol.Export;
import cn.blmdz.rabbit.item.service.GalaxyItemReadService;
import cn.blmdz.wolf.parana.shop.model.Shop;
import cn.blmdz.wolf.parana.shop.service.ShopReadService;
import lombok.extern.slf4j.Slf4j;

/**
 * Date: 7/19/16
 * Time: 1:22 PM
 * Author: 2016年 <a href="mailto:d@terminus.io">张成栋</a>
 */
@Slf4j
public class GalaxyShopCacher {
    private final LoadingCache<Long, Long> shopItemCountCache;
    private final LoadingCache<Long, Shop> shopCache;

    @Autowired
    public GalaxyShopCacher(final ShopReadService shopReadService,
                            final GalaxyItemReadService galaxyItemReadService,
                            @Value("${cache.shop.duration-in-minutes:3}") Long duration) {
        this.shopItemCountCache = CacheBuilder.newBuilder()
                .expireAfterWrite(duration, TimeUnit.MINUTES)
                .build(new CacheLoader<Long, Long>() {
                    @Override
                    public Long load(Long shopId) throws Exception {
                        Response<Long> tryCount = galaxyItemReadService.countItemByShop(shopId);
                        if (tryCount.isSuccess()) {
                            return tryCount.getResult();
                        }
                        log.error("fail to count item by shop id:{}, cause:{}", shopId, tryCount.getError());
                        throw new ServiceException(tryCount.getError());
                    }
                });

        this.shopCache = CacheBuilder.newBuilder()
                .expireAfterWrite(duration, TimeUnit.MINUTES)
                .build(new CacheLoader<Long, Shop>() {
                    @Override
                    public Shop load(Long shopId) throws Exception {
                        Response<Shop> findShop = shopReadService.findById(shopId);
                        if (findShop.isSuccess()) {
                            return findShop.getResult();
                        }
                        log.error("fail to find shop by id:{}, cause:{}", shopId, findShop.getError());
                        throw new ServiceException(findShop.getError());
                    }
                });
    }

    @Export("shopId")
    public Shop findById(Long shopId) {
        return shopCache.getUnchecked(shopId);
    }

    @Export("shopId")
    public Long countItemByShopId(Long shopId) {
        return shopItemCountCache.getUnchecked(shopId);
    }
}
