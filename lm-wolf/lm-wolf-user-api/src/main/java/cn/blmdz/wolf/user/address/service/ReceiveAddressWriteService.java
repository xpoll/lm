package cn.blmdz.wolf.user.address.service;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.user.address.model.ReceiveAddress;

public interface ReceiveAddressWriteService {
   Response createReceiveAddress(ReceiveAddress var1);

   Response updateReceiveAddress(ReceiveAddress var1);

   Response makeDefault(Long var1, Long var2);

   Response deleteAddressByAddressIdAndUserId(Long var1, Long var2);
}
