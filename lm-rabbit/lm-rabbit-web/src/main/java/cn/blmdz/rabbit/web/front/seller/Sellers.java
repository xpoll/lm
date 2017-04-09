package cn.blmdz.rabbit.web.front.seller;

import java.util.Objects;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import com.google.common.base.Optional;

import cn.blmdz.home.common.exception.JsonResponseException;
import cn.blmdz.home.common.model.BaseUser;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.hunt.common.UserUtil;
import cn.blmdz.rabbit.user.model.Seller;
import cn.blmdz.rabbit.user.service.SellerReadService;
import cn.blmdz.rabbit.user.service.SellerWriteService;
import cn.blmdz.wolf.common.utils.RespHelper;
import lombok.extern.slf4j.Slf4j;

/**
 * @author Effet
 */
@Slf4j
@RestController
@RequestMapping("/api/seller")
public class Sellers {

    private final SellerReadService sellerReadService;

    private final SellerWriteService sellerWriteService;

    @Autowired
    public Sellers(SellerReadService sellerReadService, SellerWriteService sellerWriteService) {
        this.sellerReadService = sellerReadService;
        this.sellerWriteService = sellerWriteService;
    }

    @RequestMapping(value = "apply", method = RequestMethod.POST)
    public Boolean applySeller(@RequestBody Seller seller) {
        BaseUser user = UserUtil.getCurrentUser();
        if (user == null) {
            log.warn("apply to seller failed, user not login");
            throw new JsonResponseException(401, "user.not.login");
        }
        Response<Optional<Seller>> sellerFindResp = sellerReadService.findSellerByUserId(user.getId());
        if (!sellerFindResp.isSuccess()) {
            log.error("find existing seller failed, userId={}, error={}", user.getId(), sellerFindResp.getError());
            throw new JsonResponseException(500, sellerFindResp.getError());
        }
        Seller exist = sellerFindResp.getResult().orNull();
        if (exist == null) {
            // not exist case
            Seller toCreate = new Seller();
            toCreate.setUserId(user.getId());
            toCreate.setUserName(user.getName());
            toCreate.setStatus(0);
            toCreate.setExtra(seller.getExtra());
            RespHelper.<Long>or500(sellerWriteService.createSeller(toCreate));
            return Boolean.TRUE;
        } else {
            if (Objects.equals(exist.getStatus(), 0) || Objects.equals(exist.getStatus(), -1)) {
                Seller toUpdate = new Seller();
                toUpdate.setId(exist.getId());
                toUpdate.setStatus(0);
                toUpdate.setExtra(seller.getExtra());
                return RespHelper.<Boolean>or500(sellerWriteService.updateSeller(toUpdate));
            } else {
                log.error("seller apply status neither init nor rejected, cannot update apply info");
                throw new JsonResponseException(500, "seller.apply.status.not.allow.reapply");
            }
        }
    }
}
