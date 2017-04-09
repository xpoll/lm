package cn.blmdz.rabbit.web.front.seller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import com.google.common.collect.Lists;

import cn.blmdz.home.common.exception.JsonResponseException;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.home.common.util.Params;
import cn.blmdz.rabbit.common.enums.UserRole;
import cn.blmdz.rabbit.common.enums.UserStatus;
import cn.blmdz.rabbit.common.enums.UserType;
import cn.blmdz.rabbit.user.model.SellerRole;
import cn.blmdz.rabbit.user.model.SubSeller;
import cn.blmdz.rabbit.user.service.SellerReadService;
import cn.blmdz.rabbit.user.service.SellerRoleReadService;
import cn.blmdz.rabbit.user.service.SellerWriteService;
import cn.blmdz.rabbit.web.core.component.UserCacheCleaner;
import cn.blmdz.rabbit.web.util.SellerUtils;
import cn.blmdz.wolf.common.utils.EncryptUtil;
import cn.blmdz.wolf.common.utils.RespHelper;
import cn.blmdz.wolf.user.model.User;
import cn.blmdz.wolf.user.service.UserReadService;
import cn.blmdz.wolf.user.service.UserWriteService;
import lombok.Data;
import lombok.val;
import lombok.extern.slf4j.Slf4j;

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
            SellerRole sellerRole = RespHelper.<SellerRole>or500(sellerRoleReadService.findById(sub.getRoleId()));
            role = new SubSeller.SubSellerRole();
            role.setId(sub.getRoleId());
            role.setName(sellerRole.getName());
        }

        Long userId = RespHelper.<Long>or500(userWriteService.create(user));

        SubSeller subSeller = new SubSeller();
        subSeller.setUserId(userId);
        subSeller.setUserName(sub.getUsername());
        subSeller.setShopId(shopId);
        subSeller.setStatus(1);
        subSeller.setRoles(Lists.newArrayList(role));
        RespHelper.<Long>or500(sellerWriteService.createSubSeller(subSeller));
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
            SellerRole sellerRole = RespHelper.<SellerRole>or500(sellerRoleReadService.findById(sub.getRoleId()));
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
