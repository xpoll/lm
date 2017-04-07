package cn.blmdz.rabbit.user.service;

import java.util.Map;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.rabbit.user.model.Business;

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
