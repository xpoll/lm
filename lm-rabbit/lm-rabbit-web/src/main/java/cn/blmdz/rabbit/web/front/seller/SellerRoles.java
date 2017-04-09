package cn.blmdz.rabbit.web.front.seller;

import static cn.blmdz.wolf.common.utils.RespHelper.or500;

import java.util.List;
import java.util.Objects;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import cn.blmdz.home.common.exception.JsonResponseException;
import cn.blmdz.hunt.engine.ThreadVars;
import cn.blmdz.rabbit.user.model.SellerRole;
import cn.blmdz.rabbit.user.service.SellerRoleReadService;
import cn.blmdz.rabbit.user.service.SellerRoleWriteService;
import cn.blmdz.rabbit.web.util.SellerUtils;
import cn.blmdz.wolf.common.utils.RespHelper;

/**
 * @author Effet
 */
@RestController
@RequestMapping("/api/seller/role")
public class SellerRoles {

    private final SellerRoleReadService sellerRoleReadService;

    private final SellerRoleWriteService sellerRoleWriteService;

    @Autowired
    public SellerRoles(SellerRoleReadService sellerRoleReadService, SellerRoleWriteService sellerRoleWriteService) {
        this.sellerRoleReadService = sellerRoleReadService;
        this.sellerRoleWriteService = sellerRoleWriteService;
    }

    /**
     * 创建卖家角色
     *
     * @param role 卖家角色
     * @return 角色主键 ID
     */
    @RequestMapping(value = "", method = RequestMethod.POST)
    public Long createRole(@RequestBody SellerRole role) {
        role.setShopId(SellerUtils.getLoggedShopId());
        role.setAppKey(ThreadVars.getAppKey());
        role.setStatus(1);
        return or500(sellerRoleWriteService.createRole(role));
    }

    /**
     * 更新卖家角色
     *
     * @param id         角色 ID
     * @param sellerRole 角色授权内容
     * @return 是否更新成功
     */
    @RequestMapping(value = "/{id}", method = RequestMethod.PUT)
    public Boolean updateRole(@PathVariable Long id, @RequestBody SellerRole sellerRole) {
        SellerRole existRole = RespHelper.<SellerRole>orServEx(sellerRoleReadService.findById(id));
        if (existRole == null) {
            throw new JsonResponseException(500, "seller.role.not.exist");
        }
        if (!Objects.equals(existRole.getShopId(), SellerUtils.getLoggedShopId())) {
            throw new JsonResponseException(500, "seller.role.no.permission");
        }
        sellerRole.setId(id);
        sellerRole.setShopId(null); // prevent update
        sellerRole.setAppKey(null); // prevent update
        sellerRole.setStatus(null); // prevent update
        return or500(sellerRoleWriteService.updateRole(sellerRole));
    }

    @RequestMapping(value = "/{id}", method = RequestMethod.DELETE)
    public Boolean deleteRole(@PathVariable Long id) {
        SellerRole toUpdate = new SellerRole();
        toUpdate.setId(id);
        toUpdate.setStatus(-1);
        return or500(sellerRoleWriteService.updateRole(toUpdate));
    }

    /**
     * 拿所有合法角色
     *
     * @return 角色列表
     */
    @RequestMapping(value = "/all", method = RequestMethod.GET)
    public List<SellerRole> findAllRoles() {
        Long shopId = SellerUtils.getLoggedShopId();
        return RespHelper.<List<SellerRole>>or500(sellerRoleReadService.findByShopIdAndStatus(ThreadVars.getAppKey(), shopId, 1));
    }
}
