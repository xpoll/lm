package io.terminus.galaxy.web.admin.seller;

import com.google.common.base.Optional;
import com.google.common.base.Strings;
import io.terminus.common.exception.JsonResponseException;
import io.terminus.common.model.Response;
import io.terminus.galaxy.user.model.Seller;
import io.terminus.galaxy.user.service.SellerReadService;
import io.terminus.galaxy.user.service.SellerWriteService;
import io.terminus.galaxy.web.core.component.UserCacheCleaner;
import io.terminus.parana.common.utils.RespHelper;
import io.terminus.parana.shop.model.Shop;
import io.terminus.parana.shop.service.ShopReadService;
import io.terminus.parana.shop.service.ShopWriteService;
import io.terminus.parana.user.model.User;
import io.terminus.parana.user.service.UserReadService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.Map;
import java.util.Objects;

/**
 * @author Effet
 */
@Slf4j
@RestController
@RequestMapping("/api/seller")
public class Sellers {

    private final UserReadService<User> userReadService;

    private final SellerReadService sellerReadService;

    private final SellerWriteService sellerWriteService;

    private final ShopReadService shopReadService;

    private final ShopWriteService shopWriteService;

    private final UserCacheCleaner userCacheCleaner;

    @Autowired
    public Sellers(UserReadService<User> userReadService,
                   SellerReadService sellerReadService,
                   SellerWriteService sellerWriteService,
                   ShopReadService shopReadService,
                   ShopWriteService shopWriteService,
                   UserCacheCleaner userCacheCleaner) {
        this.userReadService = userReadService;
        this.sellerReadService = sellerReadService;
        this.sellerWriteService = sellerWriteService;
        this.shopReadService = shopReadService;
        this.shopWriteService = shopWriteService;
        this.userCacheCleaner = userCacheCleaner;
    }

    @RequestMapping(value = "", method = RequestMethod.POST)
    public Long createSeller(@RequestBody Seller seller) {
        Long userId = seller.getUserId();
        if (userId == null) {
            log.error("create seller failed, userId not specified");
            throw new JsonResponseException(400, "seller.create.fail.no.user.id");
        }
        Response<Optional<Seller>> findSellerResp = sellerReadService.findSellerByUserId(userId);
        if (!findSellerResp.isSuccess()) {
            log.error("find seller exist failed, userId={}, error={}", userId, findSellerResp.getError());
            throw new JsonResponseException(500, "seller.create.fail.find.exist.fail");
        }
        if (findSellerResp.getResult().isPresent()) {
            log.warn("seller already exist, userId={}", userId);
            throw new JsonResponseException(500, "seller.create.fail.already.exist");
        }

        User user = RespHelper.or500(userReadService.findById(userId));

        Seller toCreate = new Seller();
        toCreate.setUserId(userId);
        toCreate.setUserName(getUserName(user));
        toCreate.setStatus(1); // 运营创建默认为审核通过
        toCreate.setExtra(seller.getExtra());
        return RespHelper.or500(sellerWriteService.createSeller(toCreate));
    }

    private String getUserName(User user) {
        if (!Strings.isNullOrEmpty(user.getName())) {
            return user.getName();
        }
        if (!Strings.isNullOrEmpty(user.getEmail())) {
            return user.getEmail();
        }
        if (!Strings.isNullOrEmpty(user.getMobile())) {
            return user.getMobile();
        }
        return "";
    }

    @RequestMapping(value = "/{userId}/approval", method = RequestMethod.PUT)
    public Boolean approvalSeller(@PathVariable Long userId) {
        Response<Optional<Seller>> findSellerResp = sellerReadService.findSellerByUserId(userId);
        if (!findSellerResp.isSuccess()) {
            log.error("find seller exist failed, userId={}, error={}", userId, findSellerResp.getError());
            throw new JsonResponseException(500, "seller.approval.fail.find.exist.fail");
        }
        Seller exist = findSellerResp.getResult().orNull();
        if (exist == null) {
            log.warn("seller not exist, userId={}", userId);
            throw new JsonResponseException(500, "seller.approval.fail.not.exist");
        }
        // TODO: check legal status
        Seller toUpdate = new Seller();
        toUpdate.setId(exist.getId());
        toUpdate.setStatus(1);
        Boolean result = RespHelper.or500(sellerWriteService.updateSeller(toUpdate));

        // clean user cache
        userCacheCleaner.clean(userId);

        return result;
    }

    @RequestMapping(value = "/{userId}/reject", method = RequestMethod.PUT)
    public Boolean rejectSeller(@PathVariable Long userId, @RequestParam String reason) {
        Response<Optional<Seller>> findSellerResp = sellerReadService.findSellerByUserId(userId);
        if (!findSellerResp.isSuccess()) {
            log.error("find seller exist failed, userId={}, error={}", userId, findSellerResp.getError());
            throw new JsonResponseException(500, "seller.reject.fail.find.exist.fail");
        }
        Seller exist = findSellerResp.getResult().orNull();
        if (exist == null) {
            log.warn("seller not exist, userId={}", userId);
            throw new JsonResponseException(500, "seller.reject.fail.not.exist");
        }
        // TODO: check legal status
        Seller toUpdate = new Seller();
        toUpdate.setId(exist.getId());
        toUpdate.setStatus(-1);
        Map<String, String> extra = exist.getExtra();
        extra.put("reason", reason);
        toUpdate.setExtra(extra);
        return RespHelper.or500(sellerWriteService.updateSeller(toUpdate));
    }

    @RequestMapping(value = "/{userId}/frozen", method = RequestMethod.PUT)
    public Boolean frozenSeller(@PathVariable Long userId) {
        Response<Optional<Seller>> findSellerResp = sellerReadService.findSellerByUserId(userId);
        if (!findSellerResp.isSuccess()) {
            log.error("find seller exist failed, userId={}, error={}", userId, findSellerResp.getError());
            throw new JsonResponseException(500, "seller.frozen.fail.find.exist.fail");
        }
        Seller exist = findSellerResp.getResult().orNull();
        if (exist == null) {
            log.warn("seller not exist, userId={}", userId);
            throw new JsonResponseException(500, "seller.frozen.fail.not.exist");
        }
        // TODO: check legal status
        Seller toUpdate = new Seller();
        toUpdate.setId(exist.getId());
        toUpdate.setStatus(-2); // 冻结
        Boolean result = RespHelper.or500(sellerWriteService.updateSeller(toUpdate));

        // clean user cache
        userCacheCleaner.clean(userId);

        return result;
    }

    @RequestMapping(value = "/{userId}/unfrozen", method = RequestMethod.PUT)
    public Boolean unfrozenSeller(@PathVariable Long userId) {
        Response<Optional<Seller>> findSellerResp = sellerReadService.findSellerByUserId(userId);
        if (!findSellerResp.isSuccess()) {
            log.error("find seller exist failed, userId={}, error={}", userId, findSellerResp.getError());
            throw new JsonResponseException(500, "seller.unfrozen.fail.find.exist.fail");
        }
        Seller exist = findSellerResp.getResult().orNull();
        if (exist == null) {
            log.warn("seller not exist, userId={}", userId);
            throw new JsonResponseException(500, "seller.unfrozen.fail.not.exist");
        }
        // TODO: check legal status
        Seller toUpdate = new Seller();
        toUpdate.setId(exist.getId());
        toUpdate.setStatus(1); // 恢复成正常状态
        Boolean result = RespHelper.or500(sellerWriteService.updateSeller(toUpdate));

        // clean user cache
        userCacheCleaner.clean(userId);

        return result;
    }

    @RequestMapping(value = "/{userId}/shop", method = RequestMethod.PUT)
    public Boolean createShop(@PathVariable Long userId) {
        Response<Optional<Seller>> findSellerResp = sellerReadService.findSellerByUserId(userId);
        if (!findSellerResp.isSuccess()) {
            log.error("find seller exist failed, userId={}, error={}", userId, findSellerResp.getError());
            throw new JsonResponseException(500, "seller.create.shop.fail.find.exist.fail");
        }
        Seller exist = findSellerResp.getResult().orNull();
        if (exist == null) {
            log.warn("seller not exist, userId={}", userId);
            throw new JsonResponseException(500, "seller.create.shop.fail.not.exist");
        }
        if (!Objects.equals(exist.getStatus(), 1)) {
            // 审核没有通过
            log.warn("seller not approved, userId={}", userId);
            throw new JsonResponseException(500, "seller.create.shop.fail.not.approved");
        }
        // shopReadService.findByUserId(userId); // TODO: 需要能判断店铺是否存在的接口
        Shop toCreate = new Shop();
        toCreate.setUserId(userId);
        toCreate.setUserName(exist.getUserName());
//        toCreate.setName(exist.getShopName());
        toCreate.setName(exist.getExtra().get("shopName"));
        toCreate.setType(1);
        toCreate.setStatus(1);
        toCreate.setPhone(exist.getExtra().get("phone"));
        Long shopId = RespHelper.or500(shopWriteService.create(toCreate));

        Seller toUpdateSeller = new Seller();
        toUpdateSeller.setId(exist.getId());
        toUpdateSeller.setShopId(shopId);
        toUpdateSeller.setShopName(exist.getExtra().get("shopName"));
        return RespHelper.or500(sellerWriteService.updateSeller(toUpdateSeller));
    }
}
