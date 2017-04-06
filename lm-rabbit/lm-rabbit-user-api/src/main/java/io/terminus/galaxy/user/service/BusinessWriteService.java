package io.terminus.galaxy.user.service;

import io.terminus.common.model.Response;
import io.terminus.galaxy.user.model.Business;

import java.util.Map;

/**
 * Created by liushaofei on 16/8/9.
 */
public interface BusinessWriteService {

    /**
     * 单个插入
     * @param business
     * @return
     */
    Response<Boolean> insert(Business business);

    /**
     * 单个删除
     * @param business
     * @return
     */
    Response<Boolean> delete(Business business);

    /**
     * 单个修改
     * @param map
     * @return
     */
    Response<Boolean> update(Map<String, Object> map);
}
