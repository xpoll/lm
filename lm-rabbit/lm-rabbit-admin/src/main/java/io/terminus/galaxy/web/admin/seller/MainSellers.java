package io.terminus.galaxy.web.admin.seller;

import io.terminus.common.exception.JsonResponseException;
import io.terminus.galaxy.user.model.MainSellerRole;
import io.terminus.galaxy.user.model.Seller;
import io.terminus.galaxy.user.service.MainSellerRoleReadService;
import io.terminus.galaxy.user.service.SellerReadService;
import io.terminus.galaxy.user.service.SellerWriteService;
import io.terminus.parana.common.utils.RespHelper;
import lombok.extern.slf4j.Slf4j;
import lombok.val;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.HashMap;
import java.util.Map;

/**
 * @author Effet
 */
@Slf4j
@RestController
@RequestMapping("/api/main-seller")
public class MainSellers {

    @Autowired
    private SellerReadService sellerReadService;

    @Autowired
    private SellerWriteService sellerWriteService;

    @Autowired
    private MainSellerRoleReadService mainSellerRoleReadService;

    @RequestMapping(value = "/{userId}", method = RequestMethod.PUT, produces = MediaType.APPLICATION_JSON_VALUE)
    public void changeRole(@PathVariable Long userId, @RequestParam Long roleId) {
        val rSeller = sellerReadService.findSellerByUserId(userId);
        if (!rSeller.isSuccess()) {
            log.error("find seller failed, userId={}, error={}", userId, rSeller.getError());
            throw new JsonResponseException(rSeller.getError());
        }
        Seller seller = rSeller.getResult().orNull();
        if (seller == null) {
            return;
        }
        val rRole = mainSellerRoleReadService.findById(roleId);
        if (!rRole.isSuccess()) {
            log.warn("find role of seller failed, roleId={}, error={}", roleId, rRole.getError());
            throw new JsonResponseException(rRole.getError());
        }
        MainSellerRole role = rRole.getResult();

        Map<String, String> map = seller.getExtra();
        if (map == null) {
            map = new HashMap<>();
        }
        map.put("roleId", roleId.toString());
        map.put("roleName", role.getName());

        seller.setExtra(map);
        RespHelper.or500(sellerWriteService.updateSeller(seller));
    }
}
