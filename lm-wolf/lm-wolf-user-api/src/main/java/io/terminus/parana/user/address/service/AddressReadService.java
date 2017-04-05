package io.terminus.parana.user.address.service;

import io.terminus.common.model.Response;
import io.terminus.pampas.client.Export;

public interface AddressReadService {
   @Export
   Response provinces();

   @Export(
      paramNames = {"provinceId"}
   )
   Response citiesOf(Integer var1);

   @Export(
      paramNames = {"cityId"}
   )
   Response regionsOf(Integer var1);

   @Export(
      paramNames = {"regionId"}
   )
   Response streetsOf(Integer var1);

   @Export(
      paramNames = {"id"}
   )
   Response childAddressOf(Integer var1);

   @Export(
      paramNames = {"id"}
   )
   Response findById(Integer var1);

   @Export(
      paramNames = {"addressId"}
   )
   Response ancestorsOf(Integer var1);

   @Export(
      paramNames = {"addressId"}
   )
   Response ancestorOfAddresses(Integer var1);

   @Export(
      paramNames = {"pid"}
   )
   Response getTreeOf(Integer var1);

   Response findByName(String var1);
}
