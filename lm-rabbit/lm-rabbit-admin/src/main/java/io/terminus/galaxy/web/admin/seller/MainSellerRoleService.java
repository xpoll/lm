package io.terminus.galaxy.web.admin.seller;

import com.google.common.base.Throwables;
import com.google.common.collect.Lists;
import io.terminus.common.exception.ServiceException;
import io.terminus.common.model.BaseUser;
import io.terminus.common.model.Paging;
import io.terminus.common.model.Response;
import io.terminus.galaxy.user.model.MainSellerRole;
import io.terminus.galaxy.user.service.MainSellerRoleReadService;
import io.terminus.pampas.client.Export;
import io.terminus.parana.common.utils.RespHelper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import javax.annotation.Nullable;
import java.util.Objects;

/**
 * @author Effet
 */
@Slf4j
@Component
public class MainSellerRoleService {

    private final MainSellerRoleReadService mainSellerRoleReadService;

    @Autowired
    public MainSellerRoleService(MainSellerRoleReadService mainSellerRoleReadService) {
        this.mainSellerRoleReadService = mainSellerRoleReadService;
    }

    /**
     * 运营角色分页
     *
     * @param user     登陆用户
     * @param id       角色 ID
     * @param status   角色状态
     * @param pageNo   页码
     * @param pageSize 查询数量
     * @return 分页结果
     */
    @Export(paramNames = {"user", "name", "id", "status", "pageNo", "pageSize"})
    public Response<Paging<MainSellerRole>> pagination(BaseUser user, String name, Long id, Integer status, Integer pageNo, Integer pageSize) {
        try {
            if (id != null) {
                MainSellerRole role = RespHelper.orServEx(mainSellerRoleReadService.findById(id));
                if (Objects.equals(role.getStatus(), -1)) {
                    return Response.ok(Paging.<MainSellerRole>empty());
                }
                return Response.ok(new Paging<>(1L, Lists.newArrayList(role)));
            }
            return mainSellerRoleReadService.pagination(name, status, pageNo, pageSize);
        } catch (ServiceException e) {
            log.warn("paging operator roles failed, user={}, id={}, status={}, pageNo={}, pageSize={}, error={}",
                    user, id, status, pageNo, pageSize, e.getMessage());
            return Response.fail(e.getMessage());
        } catch (Exception e) {
            log.error("paging operator roles failed, user={}, id={}, status={}, pageNo={}, pageSize={}, cause:{}",
                    user, id, status, pageNo, pageSize, Throwables.getStackTraceAsString(e));
            return Response.fail("operator.role.paging.fail");
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
    public Response<MainSellerRole> findByIdForUpdate(BaseUser user, @Nullable Long id) {
        try {
            // id 为 null, 为创建页面调用, 直接返回 OPERATOR 权限树
            if (id == null) {
                MainSellerRole role = new MainSellerRole();
                return Response.ok(role);
            }
            MainSellerRole role = RespHelper.orServEx(mainSellerRoleReadService.findById(id));
            if (role == null) {
                log.warn("operator role not id={}", id);
                return Response.fail("operator.role.not.found");
            }
            return Response.ok(role);
        } catch (ServiceException e) {
            log.warn("find operator role by id={} failed, error={}", id, e.getMessage());
            return Response.fail(e.getMessage());
        } catch (Exception e) {
            log.error("find operator role by id={} failed, cause:{}",
                    id, Throwables.getStackTraceAsString(e));
            return Response.fail("operator.role.find.fail");
        }
    }
}
