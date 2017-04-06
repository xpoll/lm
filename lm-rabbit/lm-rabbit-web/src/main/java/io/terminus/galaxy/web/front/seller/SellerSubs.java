package io.terminus.galaxy.web.front.seller;

import com.google.common.collect.Lists;
import io.terminus.common.exception.JsonResponseException;
import io.terminus.common.model.Response;
import io.terminus.common.utils.Params;
import io.terminus.galaxy.common.enums.UserRole;
import io.terminus.galaxy.common.enums.UserStatus;
import io.terminus.galaxy.common.enums.UserType;
import io.terminus.galaxy.user.model.SellerRole;
import io.terminus.galaxy.user.model.SubSeller;
import io.terminus.galaxy.user.service.SellerReadService;
import io.terminus.galaxy.user.service.SellerRoleReadService;
import io.terminus.galaxy.user.service.SellerWriteService;
import io.terminus.galaxy.web.core.component.UserCacheCleaner;
import io.terminus.galaxy.web.util.SellerUtils;
import io.terminus.parana.common.utils.EncryptUtil;
import io.terminus.parana.common.utils.RespHelper;
import io.terminus.parana.user.model.User;
import io.terminus.parana.user.service.UserReadService;
import io.terminus.parana.user.service.UserWriteService;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import lombok.val;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

/**
 * @author Effet
 */
@Slf4j
@RestController
@RequestMapping("/api/seller/sub")
public class SellerSubs {

    private final UserReadService<User> userReadService;

    private final UserWriteService<User> userWriteService;

    private final SellerReadService sellerReadService;

    private final SellerWriteService sellerWriteService;

    private final SellerRoleReadService sellerRoleReadService;

    private final UserCacheCleaner userCacheCleaner;

    @Autowired
    public SellerSubs(UserReadService<User> userReadService,
                      UserWriteService<User> userWriteService,
                      SellerReadService sellerReadService,
                      SellerWriteService sellerWriteService,
                      SellerRoleReadService sellerRoleReadService,
                      UserCacheCleaner userCacheCleaner) {
        this.userReadService = userReadService;
        this.userWriteService = userWriteService;
        this.sellerReadService = sellerReadService;
        this.sellerWriteService = sellerWriteService;
        this.sellerRoleReadService = sellerRoleReadService;
        this.userCacheCleaner = userCacheCleaner;
    }

    /**
     * 商家关联子账户
     *
     * @param sub 子账户信息
     * @return 子账户用户 ID
     */
    @RequestMapping(value = "", method = RequestMethod.POST)
    public Long createSubSeller(@RequestBody SubSellerPost sub) {
        Long shopId = SellerUtils.getLoggedShopId();

        User user = new User();
        user.setName(sub.getUsername());
        user.setPassword(sub.getPassword());
        user.setType(UserType.SUB_ACCOUNT.value());
        user.setStatus(UserStatus.NORMAL.value());
        user.setRoles(Lists.newArrayList(UserRole.SELLER.name()));

        SubSeller.SubSellerRole role = null;
        if (sub.getRoleId() != null) {
            SellerRole sellerRole = RespHelper.or500(sellerRoleReadService.findById(sub.getRoleId()));
            role = new SubSeller.SubSellerRole();
            role.setId(sub.getRoleId());
            role.setName(sellerRole.getName());
        }

        Long userId = RespHelper.or500(userWriteService.create(user));

        SubSeller subSeller = new SubSeller();
        subSeller.setUserId(userId);
        subSeller.setUserName(sub.getUsername());
        subSeller.setShopId(shopId);
        subSeller.setStatus(1);
        subSeller.setRoles(Lists.newArrayList(role));
        RespHelper.or500(sellerWriteService.createSubSeller(subSeller));
        return userId;
    }

    /**
     * 商家修改关联子账户
     *
     * @param sub 子账户信息
     * @return 子账户用户 ID
     */
    @RequestMapping(value = "/{userId}", method = RequestMethod.PUT)
    public Boolean updateSubSeller(@PathVariable Long userId, @RequestBody SubSellerPost sub) {
        Long shopId = SellerUtils.getLoggedShopId();

        val subSellerResp = sellerReadService.findSubSellerByUserId(userId);
        if (!subSellerResp.isSuccess()) {
            log.warn("subSeller update failed, find exist fail, userId={}, error={}", userId, subSellerResp.getError());
            throw new JsonResponseException(subSellerResp.getError());
        }
        if (!subSellerResp.getResult().isPresent()) {
            log.warn("subSeller update failed, not exist, userId={}");
            throw new JsonResponseException("seller.sub.update.fail.not.exist");
        }
        val existSub = subSellerResp.getResult().get();

        User toUpdateUser = new User();
        toUpdateUser.setId(userId);
        toUpdateUser.setName(Params.trimToNull(sub.getUsername()));
        String password = Params.trimToNull(sub.getPassword());
        if (password != null) {
            password = EncryptUtil.encrypt(password);
            toUpdateUser.setPassword(password);
        }

        Response<Boolean> userResp = userWriteService.update(toUpdateUser);
        if (!userResp.isSuccess()) {
            log.warn("user update failed, cause:{}", userResp.getError());
            throw new JsonResponseException(userResp.getError());
        }

        if (sub.getRoleId() != null) {
            SubSeller toUpdateSub = new SubSeller();
            toUpdateSub.setId(existSub.getId());
            if (toUpdateUser.getName() != null) {
                toUpdateSub.setUserName(toUpdateUser.getName());
            }
            SubSeller.SubSellerRole role = new SubSeller.SubSellerRole();
            SellerRole sellerRole = RespHelper.or500(sellerRoleReadService.findById(sub.getRoleId()));
            role.setId(sub.getRoleId());
            role.setName(sellerRole.getName());
            toUpdateSub.setRoles(Lists.newArrayList(role));
            Response<Boolean> subUpdateResp = sellerWriteService.updateSubSeller(toUpdateSub);
            if (!subUpdateResp.isSuccess()) {
                log.warn("subSeller update failed, error={}", subUpdateResp.getError());
                throw new JsonResponseException(subUpdateResp.getError());
            }
        }

        userCacheCleaner.clean(userId);

        return Boolean.TRUE;
    }

    @Data
    public static class SubSellerPost {

        private String username;

        private String password;

        private Long roleId;
    }
}
