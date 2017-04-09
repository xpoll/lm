package cn.blmdz.rabbit.web.front.seller;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.google.common.base.Throwables;

import cn.blmdz.home.common.exception.ServiceException;
import cn.blmdz.home.common.model.Paging;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.hunt.protocol.Export;
import cn.blmdz.rabbit.user.model.SellerRole;
import cn.blmdz.rabbit.user.model.SubSeller;
import cn.blmdz.rabbit.user.service.SellerReadService;
import cn.blmdz.rabbit.user.service.SellerRoleReadService;
import cn.blmdz.rabbit.web.util.SellerUtils;
import cn.blmdz.wolf.common.model.ParanaUser;
import cn.blmdz.wolf.shop.service.ShopReadService;
import lombok.val;
import lombok.extern.slf4j.Slf4j;

/**
 * @author Effet
 */
@Slf4j
@Component
public class SellerSubService {

    private final ShopReadService shopReadService;

    private final SellerReadService sellerReadService;

    private final SellerRoleReadService sellerRoleReadService;

    @Autowired
    public SellerSubService(ShopReadService shopReadService,
            SellerReadService sellerReadService, SellerRoleReadService sellerRoleReadService) {
        this.shopReadService = shopReadService;
        this.sellerReadService = sellerReadService;
        this.sellerRoleReadService = sellerRoleReadService;
    }

    @Export(paramNames = {"user", "name", "pageNo", "pageSize"})
    public Response<Paging<SubSeller>> pagingSubSeller(ParanaUser user, String name, Integer pageNo, Integer pageSize) {
        try {
            Long shopId = SellerUtils.getLoggedShopId(user);
            val resp = sellerReadService.subSellerPagination(name, shopId, null, pageNo, pageSize);
            if (resp.isSuccess()) {
                List<Long> roleIds = new ArrayList<>();
                for (SubSeller subSeller : resp.getResult().getData()) {
                    Long roleId = subSeller.getRoleId();
                    if (roleId != null) {
                        roleIds.add(roleId);
                    }
                }
                if (!roleIds.isEmpty()) {
                    val rRole = sellerRoleReadService.findByIds(roleIds);
                    if (!rRole.isSuccess()) {
                        return Response.fail(rRole.getError());
                    }
                    Map<Long, SellerRole> map = new HashMap<>();
                    for (SellerRole role : rRole.getResult()) {
                        map.put(role.getId(), role);
                    }

                    for (SubSeller subSeller : resp.getResult().getData()) {
                        flushRoleName(subSeller, map);
                    }
                }
            }
            return resp;
        } catch (ServiceException e) {
            log.warn("paging sub seller failed, user={}, pageNo={}, pageSize={}, error={}",
                    user, pageNo, pageSize, e.getMessage());
            return Response.fail(e.getMessage());
        } catch (Exception e) {
            log.error("paging sub seller failed, user={}, pageNo={}, pageSize={}, cause:{}",
                    user, pageNo, pageSize, Throwables.getStackTraceAsString(e));
            return Response.fail("seller.sub.paging.fail");
        }
    }

    private void flushRoleName(SubSeller subSeller, Map<Long, SellerRole> map) {
        Long roleId = subSeller.getRoleId();
        List<SubSeller.SubSellerRole> roles = new ArrayList<>();
        if (roleId != null) {
            SellerRole role = map.get(roleId);
            if (role != null) {
                SubSeller.SubSellerRole r = new SubSeller.SubSellerRole();
                r.setId(roleId);
                r.setName(role.getName());
                roles.add(r);
            }
        }
        subSeller.setRoles(roles);
    }
}
