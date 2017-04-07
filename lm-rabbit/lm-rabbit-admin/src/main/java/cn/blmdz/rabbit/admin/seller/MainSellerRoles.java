package cn.blmdz.rabbit.admin.seller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import cn.blmdz.home.common.exception.JsonResponseException;
import cn.blmdz.rabbit.user.model.MainSellerRole;
import cn.blmdz.rabbit.user.service.MainSellerRoleReadService;
import cn.blmdz.rabbit.user.service.MainSellerRoleWriteService;
import cn.blmdz.wolf.common.utils.RespHelper;

import static cn.blmdz.wolf.common.utils.RespHelper.or500;

/**
 * @author Effet
 */
@RestController
@RequestMapping("/api/main-seller/role")
public class MainSellerRoles {

    private final MainSellerRoleReadService mainSellerRoleReadService;

    private final MainSellerRoleWriteService mainSellerRoleWriteService;

    @Autowired
    public MainSellerRoles(MainSellerRoleReadService MainSellerRoleReadService, MainSellerRoleWriteService MainSellerRoleWriteService) {
        this.mainSellerRoleReadService = MainSellerRoleReadService;
        this.mainSellerRoleWriteService = MainSellerRoleWriteService;
    }

    /**
     * 创建运营角色
     *
     * @param role 运营角色
     * @return 角色主键 ID
     */
    @RequestMapping(value = "", method = RequestMethod.POST)
    public Long createRole(@RequestBody MainSellerRole role) {
        role.setStatus(1);
        return or500(mainSellerRoleWriteService.createRole(role));
    }

    /**
     * 更新运营角色
     *
     * @param id   角色 ID
     * @param role 角色授权内容
     * @return 是否更新成功
     */
    @RequestMapping(value = "/{id}", method = RequestMethod.PUT)
    public Boolean updateRole(@PathVariable Long id, @RequestBody MainSellerRole role) {
        MainSellerRole existRole = RespHelper.orServEx(mainSellerRoleReadService.findById(id));
        if (existRole == null) {
            throw new JsonResponseException(500, "operator.role.not.exist");
        }
        role.setId(id);
        role.setStatus(null); // prevent update
        return or500(mainSellerRoleWriteService.updateRole(role));
    }

    @RequestMapping(value = "/{id}", method = RequestMethod.DELETE)
    public Boolean deleteRole(@PathVariable Long id) {
        MainSellerRole toUpdate = new MainSellerRole();
        toUpdate.setId(id);
        toUpdate.setStatus(-1);
        return or500(mainSellerRoleWriteService.updateRole(toUpdate));
    }

    /**
     * 拿所有合法角色
     *
     * @return 角色列表
     */
    @RequestMapping(value = "/all", method = RequestMethod.GET)
    public List<MainSellerRole> findAllRoles() {
        return RespHelper.or500(mainSellerRoleReadService.findByStatus(1));
    }
}
