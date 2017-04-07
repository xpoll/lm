package cn.blmdz.rabbit.web.front.seller;

import java.util.Objects;

import javax.annotation.Nullable;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.google.api.client.repackaged.com.google.common.base.Strings;
import com.google.common.base.Throwables;
import com.google.common.collect.Lists;

import cn.blmdz.home.common.exception.JsonResponseException;
import cn.blmdz.home.common.exception.ServiceException;
import cn.blmdz.home.common.model.Paging;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.hunt.engine.ThreadVars;
import cn.blmdz.hunt.protocol.Export;
import cn.blmdz.rabbit.user.model.SellerRole;
import cn.blmdz.rabbit.user.service.SellerRoleReadService;
import cn.blmdz.rabbit.web.util.SellerUtils;
import cn.blmdz.wolf.common.model.ParanaUser;
import cn.blmdz.wolf.common.utils.RespHelper;
import lombok.extern.slf4j.Slf4j;

/**
 * @author Effet
 */
@Slf4j
@Component
public class SellerRoleService {

    private final SellerRoleReadService sellerRoleReadService;

    @Autowired
    public SellerRoleService(SellerRoleReadService sellerRoleReadService) {
        this.sellerRoleReadService = sellerRoleReadService;
    }

    /**
     * 卖家角色分页
     *
     * @param user     登陆用户
     * @param id       角色 ID
     * @param status   角色状态
     * @param pageNo   页码
     * @param pageSize 查询数量
     * @return 分页结果
     */
    @Export(paramNames = {"user", "id", "status", "name", "pageNo", "pageSize"})
    public Response<Paging<SellerRole>> pagination(ParanaUser user, Long id, Integer status, String name, Integer pageNo, Integer pageSize) {
        try {
            Long shopId = SellerUtils.getLoggedShopId(user);
            if (id != null) {
                SellerRole role = RespHelper.orServEx(sellerRoleReadService.findById(id));
                if (role != null && Objects.equals(role.getShopId(), shopId)) {
                    if (Objects.equals(role.getStatus(), -1)) {
                        return Response.ok(Paging.<SellerRole>empty());
                    }
                    return Response.ok(new Paging<>(1L, Lists.newArrayList(role)));
                }
            }
            return sellerRoleReadService.pagination(ThreadVars.getAppKey(), shopId, status, Strings.isNullOrEmpty(name)?null:name, pageNo, pageSize);
        } catch (ServiceException e) {
            log.warn("paging seller roles failed, user={}, id={}, status={}, pageNo={}, pageSize={}, error={}",
                    user, id, status, pageNo, pageSize, e.getMessage());
            return Response.fail(e.getMessage());
        } catch (Exception e) {
            Throwables.propagateIfInstanceOf(e, JsonResponseException.class);
            log.error("paging seller roles failed, user={}, id={}, status={}, pageNo={}, pageSize={}, cause:{}",
                    user, id, status, pageNo, pageSize, Throwables.getStackTraceAsString(e));
            return Response.fail("seller.role.paging.fail");
        }
    }

    /**
     * 提供给更新角色页面
     *
     * 通过 ID 查询角色
     *
     * @param user 登陆用户
     * @param id   角色 ID
     * @return 卖家角色
     */
    @Export(paramNames = {"user", "id"})
    public Response<SellerRole> findByIdForUpdate(ParanaUser user, @Nullable Long id) {
        try {
            Long shopId = SellerUtils.getLoggedShopId(user);

            // id 为 null, 为创建页面调用, 直接返回 SELLER 权限树
            if (id == null) {
                SellerRole role = new SellerRole();
                return Response.ok(role);
            }
            SellerRole role = RespHelper.orServEx(sellerRoleReadService.findById(id));
            if (role == null) {
                log.warn("seller role not id={}", id);
                return Response.fail("seller.role.not.found");
            }
            if (!Objects.equals(shopId, role.getShopId())) {
                log.warn("seller role shopId={} not equals login user's shopId={}", role.getShopId(), shopId);
                return Response.fail("user.no.permission");
            }
            return Response.ok(role);
        } catch (ServiceException e) {
            log.warn("find seller role by id={} failed, error={}", id, e.getMessage());
            return Response.fail(e.getMessage());
        } catch (Exception e) {
            log.error("find seller role by id={} failed, cause:{}",
                    id, Throwables.getStackTraceAsString(e));
            return Response.fail("seller.role.find.fail");
        }
    }
}
