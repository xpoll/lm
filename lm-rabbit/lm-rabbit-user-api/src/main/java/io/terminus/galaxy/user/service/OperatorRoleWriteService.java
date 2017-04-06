package io.terminus.galaxy.user.service;

import io.terminus.common.model.Response;
import io.terminus.galaxy.user.model.OperatorRole;

/**
 * 运营角色读服务
 *
 * @author Effet
 */
public interface OperatorRoleWriteService {

    /**
     * 创建运营角色
     *
     * @param operatorRole 运营角色
     * @return 主键 ID
     */
    Response<Long> createRole(OperatorRole operatorRole);

    /**
     * 更新运营角色
     *
     * @param operatorRole 运营角色
     * @return 是否更新成功
     */
    Response<Boolean> updateRole(OperatorRole operatorRole);
}
