package cn.blmdz.wolf.user.impl.address.service;

import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.google.common.base.Objects;
import com.google.common.base.Preconditions;
import com.google.common.base.Throwables;

import cn.blmdz.home.common.exception.ServiceException;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.home.common.util.Arguments;
import cn.blmdz.wolf.user.address.model.ReceiveAddress;
import cn.blmdz.wolf.user.address.service.ReceiveAddressWriteService;
import cn.blmdz.wolf.user.impl.address.dao.ReceiveAddressDao;
import cn.blmdz.wolf.user.impl.manager.ReceiveAddressManager;

@Service
public class ReceiveAddressWriteServiceImpl implements ReceiveAddressWriteService {
   private static final Logger log = LoggerFactory.getLogger(ReceiveAddressWriteServiceImpl.class);
   @Autowired
   private ReceiveAddressDao receiveAddressDao;
   @Autowired
   private ReceiveAddressManager receiveAddressManager;

   public Response createReceiveAddress(ReceiveAddress tradeAddress) {
      Response<Long> resp = new Response();

      try {
         if(tradeAddress == null) {
            log.warn("receive address data is null, can not be created");
            return Response.fail("receive.address.create.fail");
         } else {
            this.receiveAddressDao.create(tradeAddress);
            resp.setResult(tradeAddress.getId());
            return resp;
         }
      } catch (Exception var4) {
         log.error("failed to create receive address cause:{}", Throwables.getStackTraceAsString(var4));
         return Response.fail("receive.address.create.fail");
      }
   }

   public Response updateReceiveAddress(ReceiveAddress tradeAddress) {
      Response<Boolean> resp = new Response();

      try {
         if(tradeAddress == null) {
            log.warn("receive address data is null, can not be updated");
            return Response.fail("trade.address.update.fail");
         } else {
            resp.setResult(this.receiveAddressDao.update(tradeAddress));
            return resp;
         }
      } catch (Exception var4) {
         log.error("failed to update receive address cause:{}", Throwables.getStackTraceAsString(var4));
         return Response.fail("receive.address.update.fail");
      }
   }

   public Response makeDefault(Long addressId, Long userId) {
      try {
         Preconditions.checkArgument(Arguments.notNull(addressId), "address.id.empty");
         List<ReceiveAddress> addressList = this.receiveAddressDao.findAddressByUserId(userId);
         boolean found = false;

         for(ReceiveAddress address : addressList) {
            if(Objects.equal(address.getId(), addressId)) {
               found = true;
               break;
            }
         }

         if(!found) {
            log.error("receive address id = {} not belong to user userId = {}", addressId, userId);
            throw new ServiceException("authorize.fail");
         } else {
            this.receiveAddressManager.makeDefault(addressId, userId, addressList);
            return Response.ok(Boolean.TRUE);
         }
      } catch (IllegalArgumentException var7) {
         log.warn("failed to make default by addressId = {}, error : {}", addressId, var7.getMessage());
         return Response.fail("receive.address.id.is.null");
      } catch (Exception var8) {
         log.error("failed to make default by addressId = {}, error:{}", addressId, Throwables.getStackTraceAsString(var8));
         return Response.fail("make.default.fail");
      }
   }

   public Response deleteAddressByAddressIdAndUserId(Long addressId, Long userId) {
      try {
         Preconditions.checkArgument(Arguments.notNull(addressId), "address.id.empty");
         Preconditions.checkArgument(Arguments.notNull(userId), "user.id.empty");
         this.receiveAddressDao.deleteAddressByAddressIdAndUserId(addressId, userId);
         return Response.ok(Boolean.TRUE);
      } catch (IllegalArgumentException var4) {
         log.warn("failed to delete receive address by receiveId = {}, error : ", addressId, var4.getMessage());
         return Response.fail("receive.address.id.is.null");
      } catch (Exception var5) {
         log.error("failed to delete receive address by receiveId = {}, userId = {}, error : {}", new Object[]{addressId, userId, Throwables.getStackTraceAsString(var5)});
         return Response.fail("delete.receive.address.failed");
      }
   }
}
