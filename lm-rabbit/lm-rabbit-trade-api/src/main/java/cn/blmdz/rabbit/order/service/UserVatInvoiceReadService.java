package cn.blmdz.rabbit.order.service;

import cn.blmdz.home.common.model.BaseUser;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.hunt.protocol.Export;
import cn.blmdz.rabbit.order.model.UserVatInvoice;

/**
 * Desc: 增值税开票信息读服务
 * Mail: hehaiyang@terminus.io
 * Date: 16/3/17
 */
public interface UserVatInvoiceReadService {

    /**
     * 获取指定用户的增值税发票定义
     *
     * @param userId 用户id
     * @return 增值税发票定义信
     */
    Response<UserVatInvoice> getByUserId(Long userId);

    /**
     * 获取当前用户的增值税发票定义
     *
     * @param user  用户
     * @return 增值税发票定义信
     */
    @Export(paramNames = {"user"})
    Response<UserVatInvoice> getByUser(BaseUser user);

}
