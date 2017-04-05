package io.terminus.parana.user.impl.address.service;

import com.google.common.base.Preconditions;
import com.google.common.base.Throwables;
import com.google.common.collect.Lists;
import io.terminus.common.model.BaseUser;
import io.terminus.common.model.Response;
import io.terminus.common.utils.Arguments;
import io.terminus.parana.user.address.model.ReceiveAddress;
import io.terminus.parana.user.address.service.ReceiveAddressReadService;
import io.terminus.parana.user.impl.address.dao.ReceiveAddressDao;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class ReceiveAddressReadServiceImpl implements ReceiveAddressReadService {
   private static final Logger log = LoggerFactory.getLogger(ReceiveAddressReadServiceImpl.class);
   @Autowired
   private ReceiveAddressDao receiveAddressDao;

   public Response findAddressByUserId(Long userId) {
      Response<List<ReceiveAddress>> resp = new Response();

      try {
         Preconditions.checkArgument(Arguments.notNull(userId), "user.id.empty");
         List<ReceiveAddress> list = this.receiveAddressDao.findAddressByUserId(userId);
         resp.setResult(list);
      } catch (IllegalArgumentException var4) {
         log.warn("failed to find receive address, userId = {}, error: {}", userId, var4.getMessage());
         resp.setResult(Lists.newArrayList());
      } catch (Exception var5) {
         log.error("failed to find receive address by userId = {}, cause: {}", userId, Throwables.getStackTraceAsString(var5));
         resp.setError("receive.address.find.fail");
      }

      return resp;
   }

   public Response findAddressByLoginUser(BaseUser baseUser) {
      return baseUser == null?Response.fail("user.not.login"):this.findAddressByUserId(baseUser.getId());
   }

   public Response findAddressById(Long id) {
      try {
         return Response.ok(this.receiveAddressDao.findById(id));
      } catch (Exception var3) {
         log.error("failed to find receive address by id = {}, cause: {}", id, Throwables.getStackTraceAsString(var3));
         return Response.fail("receive.address.find.fail");
      }
   }
}
