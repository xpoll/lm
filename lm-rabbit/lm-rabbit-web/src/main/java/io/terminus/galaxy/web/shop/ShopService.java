package io.terminus.galaxy.web.shop;

import com.google.api.client.util.Lists;
import com.google.common.base.Throwables;
import com.google.common.primitives.Longs;
import io.terminus.common.model.Response;
import io.terminus.common.utils.Splitters;
import io.terminus.galaxy.cacher.GalaxyShopCacher;
import io.terminus.pampas.client.Export;
import io.terminus.parana.common.model.ParanaUser;
import io.terminus.parana.shop.model.Shop;
import io.terminus.parana.shop.service.ShopReadService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.util.Collections;
import java.util.List;

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
