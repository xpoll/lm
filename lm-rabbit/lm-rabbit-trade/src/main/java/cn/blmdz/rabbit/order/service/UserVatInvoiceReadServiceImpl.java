package cn.blmdz.rabbit.order.service;

import static com.google.common.base.Preconditions.checkArgument;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.google.common.base.Throwables;

import cn.blmdz.home.common.model.BaseUser;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.rabbit.order.dao.UserVatInvoiceDao;
import cn.blmdz.rabbit.order.model.UserVatInvoice;
import cn.blmdz.rabbit.order.service.UserVatInvoiceReadService;
import lombok.extern.slf4j.Slf4j;

/**
 * Desc:
 * Mail: hehaiyang@terminus.io
 * Date: 16/3/17
 */
@Slf4j
@Service
public class UserVatInvoiceReadServiceImpl implements UserVatInvoiceReadService {
    @Autowired
    private UserVatInvoiceDao userVatInvoiceDao;

    @Override
    public Response<UserVatInvoice> getByUserId(Long userId) {
        Response<UserVatInvoice> result = new Response<>();
        try {
            checkArgument(userId != null, "user.id.can.not.be.empty");
            UserVatInvoice userVatInvoice = userVatInvoiceDao.findByUserId(userId);
            result.setResult(userVatInvoice);

        } catch (IllegalArgumentException e) {
            log.error("fail to query userVatInvoice with user id:{}, error:{}", userId, e.getMessage());
            result.setError(e.getMessage());
        } catch (IllegalStateException e) {
            log.error("fail to query userVatInvoice with user id:{}, error:{}", userId, e.getMessage());
            result.setError(e.getMessage());
        } catch (Exception e) {
            log.error("fail to query userVatInvoice with user id:{}, cause:{}", userId, Throwables.getStackTraceAsString(e));
            result.setError("user.vat.invoice.query.fail");
        }
        return result;
    }

    /**
     * 根据用户id获取增值税发票定义
     *
     * @param user 用户id
     * @return 增值税发票定义信
     */
    @Override
    public Response<UserVatInvoice> getByUser(BaseUser user) {
        return getByUserId(user.getId());
    }
}
