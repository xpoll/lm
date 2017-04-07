package cn.blmdz.rabbit.web.shop;

import java.util.Collections;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import com.google.api.client.util.Lists;
import com.google.common.base.Throwables;
import com.google.common.primitives.Longs;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.home.common.util.Splitters;
import cn.blmdz.hunt.protocol.Export;
import cn.blmdz.rabbit.cacher.GalaxyShopCacher;
import cn.blmdz.wolf.common.model.ParanaUser;
import cn.blmdz.wolf.parana.shop.model.Shop;
import cn.blmdz.wolf.parana.shop.service.ShopReadService;
import lombok.extern.slf4j.Slf4j;

/**
 * @author Effet
 */
@Slf4j
@Component
public class ShopService {

    private final ShopReadService shopReadService;
    private final GalaxyShopCacher galaxyShopCacher;

    @Autowired
    public ShopService(ShopReadService shopReadService, GalaxyShopCacher galaxyShopCacher) {
        this.shopReadService = shopReadService;
        this.galaxyShopCacher = galaxyShopCacher;
    }

    @Export(paramNames = {"user"})
    public Response<Shop> findMine(ParanaUser user) {
        try {
            Long shopId = user.getShopId();
            if (shopId == null) {
                log.warn("seller shopId null, userId={}", user.getId());
                return Response.fail("shop.not.exist");
            }
            return shopReadService.findById(shopId);
        } catch (Exception e) {
            log.error("find shop by loginUser={}, failed, cause:{}",
                    user, Throwables.getStackTraceAsString(e));
            return Response.fail("shop.find.fail");
        }
    }

    @Export("ids")
    public Response<List<Shop>> findShopByIds(String ids) {
        if (!StringUtils.hasText(ids)) {
            return Response.ok(Collections.<Shop>emptyList());
        }

        List<Shop> shops = Lists.newArrayList();
        for (String idStr: Splitters.COMMA.split(ids)) {
            Long id = Longs.tryParse(idStr);
            if (id != null) {
                shops.add(getShopInCache(id));
            }
        }
        return Response.ok(shops);
    }

    private Shop getShopInCache(Long shopId) {
        if (shopId == null) {
            return noShop();
        }
        try {
            Shop shop = galaxyShopCacher.findById(shopId);
            if (shop == null) {
                return noShop();
            }
            return shop;
        } catch (Exception e) {
            log.error("find shop failed, shopId={}, error={}", shopId, Throwables.getStackTraceAsString(e));
            return noShop();
        }
    }

    private Shop noShop() {
        return new Shop();
    }
}
