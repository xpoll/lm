package io.terminus.galaxy.user.service;

import io.terminus.common.model.Paging;
import io.terminus.common.model.Response;
import io.terminus.galaxy.user.model.OperatorRole;
import io.terminus.parana.user.auth.CustomRoleReadService;

import java.util.List;

/**
 * 运营角色读服务
 *
 * @author Effet
 */
public interface OperatorRoleReadService extends CustomRoleReadService<OperatorRole> {

    /**
     * 通过 ID 查询
     *
     * @param id 主键 ID
     * @return 运营角色
     */
    Response<OperatorRole> findById(Long id);

    /**
     * 通过 IDs 批量查询
     *
     * @param ids 主键 ID 列表
     * @return 运营角色列表
     */
    Response<List<OperatorRole>> findByIds(List<Long> ids);

    /**
     * 通过角色状态查询
     *
     * @param appKey 角色使用场景
     * @param status 角色状态
     * @return 运营角色列表
     */
    Response<List<OperatorRole>> findByStatus(String appKey, Integer status);

    /**
     * 分页查询
     *
     * @param appKey 角色使用场景
     * @param status 角色状态
     * @param pageNo 页码
     * @param size   查询个数
     * @return 运营角色分页
     */
    Response<Paging<OperatorRole>> pagination(String appKey, String name, Integer status, Integer pageNo, Integer size);
}
