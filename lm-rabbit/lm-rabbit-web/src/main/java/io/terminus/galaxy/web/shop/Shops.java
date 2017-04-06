package io.terminus.galaxy.web.shop;

import io.terminus.common.exception.JsonResponseException;
import io.terminus.common.model.Response;
import io.terminus.galaxy.web.util.SellerUtils;
import io.terminus.parana.shop.model.Shop;
import io.terminus.parana.shop.service.ShopWriteService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

/**
 * @author Effet
 */
@Slf4j
@RestController
@RequestMapping("/api/shop")
public class Shops {

    private final ShopWriteService shopWriteService;

    @Autowired
    public Shops(ShopWriteService shopWriteService) {
        this.shopWriteService = shopWriteService;
    }

    @RequestMapping(value = "", method = RequestMethod.PUT)
    public Boolean updateShop(@RequestBody Shop shop) {
        Long shopId = SellerUtils.getLoggedShopId();
        if (shopId == null) {
            log.warn("shopId null to update");
            return Boolean.FALSE;
        }
        Shop toUpdate = shop;
        toUpdate.setId(shopId);
        Response<Boolean> resp = shopWriteService.update(toUpdate);
        if (!resp.isSuccess()) {
            log.error("update shop failed, shopId={}, error={}", shopId, resp.getError());
            throw new JsonResponseException(500, resp.getError());
        }
        return resp.getResult();
    }
}
