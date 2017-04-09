package cn.blmdz.rabbit.admin.role;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import cn.blmdz.home.common.exception.JsonResponseException;
import cn.blmdz.hunt.engine.ThreadVars;
import cn.blmdz.rabbit.user.model.OperatorRole;
import cn.blmdz.rabbit.user.service.OperatorRoleReadService;
import cn.blmdz.rabbit.user.service.OperatorRoleWriteService;
import cn.blmdz.wolf.common.utils.RespHelper;

import static cn.blmdz.wolf.common.utils.RespHelper.or500;

/**
 * @author Effet
 */
@RestController
@RequestMapping("/api/operator/role")
public class OperatorRoles {

    private final OperatorRoleReadService operatorRoleReadService;

    private final OperatorRoleWriteService operatorRoleWriteService;

    @Autowired
    public OperatorRoles(OperatorRoleReadService OperatorRoleReadService, OperatorRoleWriteService OperatorRoleWriteService) {
        this.operatorRoleReadService = OperatorRoleReadService;
        this.operatorRoleWriteService = OperatorRoleWriteService;
    }

    /**
     * 创建运营角色
     *
     * @param role 运营角色
     * @return 角色主键 ID
     */
    @RequestMapping(value = "", method = RequestMethod.POST)
    public Long createRole(@RequestBody OperatorRole role) {
        role.setAppKey(ThreadVars.getAppKey());
        role.setStatus(1);
        return or500(operatorRoleWriteService.createRole(role));
    }

    /**
     * 更新运营角色
     *
     * @param id   角色 ID
     * @param role 角色授权内容
     * @return 是否更新成功
     */
    @RequestMapping(value = "/{id}", method = RequestMethod.PUT)
    public Boolean updateRole(@PathVariable Long id, @RequestBody OperatorRole role) {
        OperatorRole existRole = RespHelper.<OperatorRole>orServEx(operatorRoleReadService.findById(id));
        if (existRole == null) {
            throw new JsonResponseException(500, "operator.role.not.exist");
        }
        role.setId(id);
        role.setAppKey(null); // prevent update
        role.setStatus(null); // prevent update
        return or500(operatorRoleWriteService.updateRole(role));
    }

    @RequestMapping(value = "/{id}", method = RequestMethod.DELETE)
    public Boolean deleteRole(@PathVariable Long id) {
        OperatorRole toUpdate = new OperatorRole();
        toUpdate.setId(id);
        toUpdate.setStatus(-1);
        return or500(operatorRoleWriteService.updateRole(toUpdate));
    }

    /**
     * 拿所有合法角色
     *
     * @return 角色列表
     */
    @RequestMapping(value = "/all", method = RequestMethod.GET)
    public List<OperatorRole> findAllRoles() {
        return RespHelper.<List<OperatorRole>>or500(operatorRoleReadService.findByStatus(ThreadVars.getAppKey(), 1));
    }
}
