package cn.blmdz.rabbit.user.service;

import java.util.List;

import cn.blmdz.home.common.model.Paging;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.rabbit.user.model.SellerRole;
import cn.blmdz.wolf.user.auth.CustomRoleReadService;

/**
 * 商家自定义角色读服务
 *
 * @author Effet
 */
public interface SellerRoleReadService extends CustomRoleReadService<SellerRole> {

    /**
     * 通过 ID 查询
     *
     * @param id 主键 ID
     * @return 自定义角色
     */
    Response<SellerRole> findById(Long id);

    /**
     * 通过 IDs 批量查询
     *
     * @param ids 主键 ID 列表
     * @return 自定义角色列表
     */
    Response<List<SellerRole>> findByIds(List<Long> ids);

    /**
     * 通过店铺 ID 查询
     *
     * @param appKey 角色使用场景
     * @param shopId 店铺 ID
     * @param status 角色状态
     * @return 自定义角色列表
     */
    Response<List<SellerRole>> findByShopIdAndStatus(String appKey, Long shopId, Integer status);

    /**
     * 分页查询
     *
     * @param appKey 角色使用场景
     * @param shopId 店铺 ID
     * @param status 角色状态
     * @param pageNo 页码
     * @param size   查询个数
     * @return 自定义角色分页
     */
    Response<Paging<SellerRole>> pagination(String appKey, Long shopId, Integer status, String name, Integer pageNo, Integer size);
}
