package io.terminus.parana.user.address.service;

import io.terminus.common.model.Response;
import io.terminus.parana.user.address.model.ReceiveAddress;

public interface ReceiveAddressWriteService {
   Response createReceiveAddress(ReceiveAddress var1);

   Response updateReceiveAddress(ReceiveAddress var1);

   Response makeDefault(Long var1, Long var2);

   Response deleteAddressByAddressIdAndUserId(Long var1, Long var2);
}
